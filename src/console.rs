use std::{
    io::{self, Write},
    str::FromStr,
};

use scamoku::{board::Point, game::*, rule};

/// Runs the Scamoku Console.
pub fn run() {
    println!("Scamoku Console");
    let rule = {
        println!("----- STANDARD RULES -----");
        let rule_list = rule::standard::LIST;
        let rule_cnt = rule_list.len();
        for i in 0..rule_cnt {
            println!("{} - {}", i, rule_list[i].id());
        }
        println!("--------------------------");
        let rule_i: usize = ask("Please choose a rule: ", |&i| i < rule_cnt);
        rule_list[rule_i]
    };

    let RawHandle {
        msg_tx,
        event_rx,
        join_handle: _,
    } = Builder::with_rule(rule).strict().start_raw();

    let mut record = Record::from(event_rx);
    while let Ok(event) = record.update() {
        process_event(&record, &msg_tx, event);
    }
}

fn ask<T: FromStr>(msg: &str, predicate: impl Fn(&T) -> bool) -> T {
    let mut buf = String::new();
    loop {
        print!("{}", msg);
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut buf).unwrap();

        if let Ok(res) = buf.trim().parse() {
            if predicate(&res) {
                return res;
            }
        }
        eprintln!("[Error] Input mismatch");
        buf.clear();
    }
}

/// Processes an event.
fn process_event(record: &Record<Event>, msg_tx: &PlayerMsgSender, event: Event) {
    let last_side = record.last_side();
    let last_stone = record.last_stone();
    match event {
        Event::GameStart(s) => {
            println!("----- GAME SETTINGS -----");
            println!("Rule ID: {}", s.rule_id);
            println!("Variant: {}", s.variant);
            println!("Board size: {}", s.board_size);
            println!("Move timeout: {:?}", s.move_timeout);
            println!("Game timeout: {:?}", s.game_timeout);
            println!("Strict mode: {}", s.strict);
            println!("----- GAME STARTED -----");
        }
        Event::MoveRequest(remaining) => {
            let side = record.last_side().opposite();
            let stone = record.last_stone().opposite();

            println!("{}", record.board());

            let msg = match remaining {
                None => format!("[{}] [{}] Please move: ", side, stone),
                Some(remaining) => format!(
                    "[{}] [{}] Please offer your move ({} left): ",
                    side, stone, remaining
                ),
            };

            let m: Move = ask(&msg, |m| match *m {
                Move::Normal(p) | Move::DrawOffer(p) | Move::WinClaim(p) => {
                    record.board().contains_point(p)
                }
                Move::Offers(ref v) => {
                    let board = record.board();
                    v.iter().all(|&p| board.contains_point(p))
                }
                Move::PassOrAcceptDrawOffer => true,
            });
            match m {
                Move::Normal(p) => msg_tx.make_move(p),
                Move::Offers(v) => msg_tx.offer_moves(v),
                Move::PassOrAcceptDrawOffer => {
                    if record.last_move_attr() == MoveAttr::Normal {
                        msg_tx.pass()
                    } else {
                        msg_tx.accept_draw_offer()
                    }
                }
                Move::DrawOffer(p) => msg_tx.draw_move(p),
                Move::WinClaim(p) => msg_tx.claim_win(p),
            }
        }
        Event::ChoiceRequest(side, choice_set) => {
            println!("----- CHOICE REQUEST -----");
            match &choice_set {
                ChoiceSet::Message(v) => {
                    println!("Make a choice from below:");
                    for (i, msg) in v.into_iter().enumerate() {
                        println!("[{}] {}", i, msg);
                    }
                }
                ChoiceSet::Move(v) => {
                    println!("Choose a move of the opponent from below:");
                    for (i, p) in v.into_iter().enumerate() {
                        print!("[{}] {} ", i, p);
                    }
                    println!();
                }
                ChoiceSet::MoveCount { max_count } => {
                    println!("Declare the count of the fifth moves.");
                    println!("Maximum count: {}", max_count);
                }
            }
            println!("--------------------------");

            let msg = format!(
                "[{}] [{}] Please choose: ",
                side,
                record.stone_by_side(side)
            );
            let choice = ask(&msg, |&c| choice_set.contains_index(c));

            msg_tx.make_choice(choice);
        }
        Event::TimerUpdate { .. } => todo!(),
        Event::Move(p, attr) => {
            if let Some(p) = p {
                if attr == MoveAttr::DrawOffer {
                    println!(
                        "{} ({}) moved and offered a draw: {}",
                        last_side, last_stone, p
                    );
                } else {
                    println!("{} ({}) moved: {}", last_side, last_stone, p);
                }
            } else {
                println!("{} ({}) passed.", last_side, last_stone);
            }
        }
        Event::Choice(i) => {
            let (side, choice_set) = record.last_choice_data();
            let stone = record.stone_by_side(side);
            let choice_str = match choice_set {
                ChoiceSet::Message(v) => v[i].clone(),
                ChoiceSet::Move(v) => v[i].to_string(),
                ChoiceSet::MoveCount { .. } => format!("move count: {}", i),
            };
            println!("{} ({}) made a choice: {}", side, stone, choice_str);
        }
        Event::StoneSwap => println!("The stones are swapped."),
        Event::GameEnd(res) => {
            println!("----- GAME ENDED -----");
            if let Some(side) = res.winning_side {
                println!("The winner: {}", side);
            } else {
                println!("The game ended in a draw.");
            }
            println!("Reason: {}", res.kind);
        }
        Event::Error(side, msg) => {
            eprintln!("[Error] [{}] {}", side, msg);
        }
    }
}

#[derive(Debug, Clone)]
enum Move {
    Normal(Point),
    Offers(Vec<Point>),
    PassOrAcceptDrawOffer,
    DrawOffer(Point),
    WinClaim(Point),
}

use anyhow::*;

impl FromStr for Move {
    type Err = Error;

    fn from_str(s: &str) -> Result<Move, Error> {
        let bytes = s.as_bytes();
        let len = bytes.len();
        ensure!(len != 0, "empty input");
        if bytes[0] == b'!' {
            if len == 1 {
                // '!' alone to pass or accept a draw offer.
                return Ok(Move::PassOrAcceptDrawOffer);
            }
            // Starting with '!' to move and offer a draw.
            return Ok(Move::DrawOffer(s[1..].parse()?));
        }
        Ok(if bytes[len - 1] == b'!' {
            // Ending with '!' to claim a win.
            Move::WinClaim(s[..len - 1].parse()?)
        } else if bytes.contains(&b',') {
            let mut v = Vec::new();
            for p in s.split(',') {
                v.push(p.trim().parse()?);
            }
            Move::Offers(v)
        } else {
            // Plain to make an actual move.
            Move::Normal(s.parse()?)
        })
    }
}
