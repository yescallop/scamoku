use std::{
    io::{self, Write},
    str::FromStr,
    sync::mpsc::Receiver,
};

use super::{game::*, rule};

/// Logs all the events from a receiver.
pub fn log_events(event_rx: Receiver<Event>) {
    for e in event_rx {
        match e {
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
            Event::MoveRequest(side, ord) => match ord {
                None => println!("Move requested from {}", side),
                Some(OfferOrd { ord, total }) => {
                    println!("Move offer ({}/{}) requested from {}", ord + 1, total, side)
                }
            },
            Event::ChoiceRequest(side, choice_set) => {
                println!("Choice requested from {}: {:?}", side, choice_set);
            }
            Event::DeadlineUpdate(side, i) => println!("Deadline updated for {}: {:?}", side, i),
            Event::PlayerMsg(side, msg) => {
                println!("Message sent by {}: {:?}", side, msg);
            }
            Event::Move(side, stone, p, attr) => {
                if let Some(p) = p {
                    if attr == MoveAttr::DrawOffer {
                        println!("{} ({}) moved and offered a draw: {}", side, stone, p);
                    } else {
                        println!("{} ({}) moved: {}", side, stone, p);
                    }
                } else {
                    println!("{} ({}) passed.", side, stone);
                }
            }
            Event::Choice(side, choice_set, i) => {
                let choice_str = match choice_set {
                    // Figure out if there's a safe way without swapping to achieve this.
                    ChoiceSet::Message(mut v) => v.swap_remove(i),
                    ChoiceSet::Move(v) => v[i].to_string(),
                    ChoiceSet::MoveCount { .. } => format!("move count: {}", i),
                };
                println!("Choice made by {}: {}", side, choice_str);
            }
            Event::StoneSwap => {
                println!("The stones are swapped");
            }
            Event::GameEnd(res) => {
                println!("----- GAME ENDED -----");
                if let Some(side) = res.winning_side {
                    println!("The winner: {}", side);
                } else {
                    println!("The game ended in a draw.");
                }
                println!("Reason: {}", res.kind);
            }
            Event::Error(side, player_msg, msg) => {
                eprintln!("Error occurred by {} for {:?}: {}", side, player_msg, msg);
            }
        }
    }
}

/// Runs the Scamoku Console.
pub fn run() {
    println!("Scamoku Console");
    let _rule = {
        println!("----- Standard Rules -----");
        let rule_list = rule::standard::LIST;
        let rule_cnt = rule_list.len();
        for i in 0..rule_cnt {
            println!("{} - {}", i, rule_list[i].id());
        }
        let rule_i: usize = ask("Please choose a rule: ", |i| i < rule_cnt);
        rule_list[rule_i]
    };
    todo!()
}

fn ask<T: FromStr + Copy>(msg: &str, predicate: impl Fn(T) -> bool) -> T {
    loop {
        print!("{}", msg);
        io::stdout().flush().unwrap();

        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();

        let res = buf.trim().parse();
        if let Ok(res) = res {
            if predicate(res) {
                return res;
            }
        }
        eprintln!("[Error] Input mismatch");
    }
}
