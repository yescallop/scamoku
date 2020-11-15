use std::sync::mpsc::Receiver;

use crate::board::*;
use crate::game::*;
use crate::rule::standard::*;

#[test]
fn standard_gomoku() {
    let Handle {
        player_handles: ((tx1, _), (tx2, _)),
        event_rx,
        join_handle,
    } = Builder::with_rule(StandardGomoku).strict().start();
    for i in 0..5 {
        tx1.make_move(Some(Point::new(i, i)));
        tx2.make_move(Some(Point::new(i + 1, i)));
    }

    log_events(event_rx);
    assert_eq!(
        join_handle.join().unwrap(),
        GameResult {
            kind: GameResultKind::RowCompleted,
            winning_side: Some((Side::First, Stone::Black)),
        }
    );
}

#[test]
fn freestyle_gomoku() {
    let Handle {
        player_handles: ((tx1, _), (tx2, _)),
        event_rx,
        join_handle,
    } = Builder::with_rule(FreestyleGomoku).start();
    for i in 0..5 {
        tx1.make_move(Some(Point::new(if i == 4 { 5 } else { i }, 0)));
        tx2.make_move(Some(Point::new(i * 2, 1)));
    }
    tx1.claim_win(Point::new(4, 0));

    log_events(event_rx);
    assert_eq!(
        join_handle.join().unwrap(),
        GameResult {
            kind: GameResultKind::RowCompleted,
            winning_side: Some((Side::First, Stone::Black)),
        }
    );
}

fn log_events(event_rx: Receiver<Event>) {
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
            Event::MoveRequest(_, _) => {}
            Event::ChoiceRequest(_, _) => {}
            Event::DeadlineUpdate(_) => {}
            Event::PlayerMsg(_, _) => {}
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
            Event::Choice(_, _, _) => {}
            Event::StoneSwap => {
                println!("The stones are swapped");
            }
            Event::GameEnd(res) => {
                println!("----- GAME ENDED -----");
                if let Some((side, stone)) = res.winning_side {
                    println!("The winner: {} ({})", side, stone);
                } else {
                    println!("The game ended in a draw.");
                }
                println!("Reason: {}", res.kind);
            }
            Event::Error(side, msg) => {
                println!("Error from {}: {}", side, msg);
            }
        }
    }
}
