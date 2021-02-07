use std::sync::mpsc::Receiver;

use crate::{board::*, console, game::*, rule::standard::*};

#[test]
fn point() {
    assert_eq!(Point::new(0, 0), "A1".parse().unwrap());
    assert_eq!(Point::new(26, 9), "aa10".parse().unwrap());
    for x in 0..1000 {
        let p = Point::new(x, 0);
        assert_eq!(p, p.to_string().parse().unwrap());
    }
}

#[test]
fn standard_gomoku() {
    let Handle {
        player_handles: ((tx1, _), (tx2, _)),
        event_rx,
        join_handle,
    } = Builder::with_rule(&StandardGomoku).strict().start();
    for i in 0..5 {
        tx1.make_move((i, i).into());
        tx2.make_move((i + 1, i).into());
    }

    console::log_events(event_rx);
    assert_eq!(
        join_handle.join().unwrap(),
        GameResult {
            kind: GameResultKind::RowCompleted,
            winning_side: Some((Side::First, Stone::Black)),
        }
    );
}

#[test]
fn freestyle_gomoku_raw() {
    let RawHandle {
        msg_tx,
        event_rx,
        join_handle,
    } = Builder::with_rule(&FreestyleGomoku).start_raw();
    for i in 0..5 {
        msg_tx.make_move((if i == 4 { 5 } else { i }, 0).into());
        msg_tx.make_move((i * 2, 1).into());
    }
    msg_tx.claim_win((4, 0).into());

    console::log_events(event_rx);
    assert_eq!(
        join_handle.join().unwrap(),
        GameResult {
            kind: GameResultKind::RowCompleted,
            winning_side: Some((Side::First, Stone::Black)),
        }
    );
}

#[test]
fn errors() {
    let Handle {
        player_handles: ((_tx1, _), (tx2, _)),
        event_rx,
        join_handle: _,
    } = Builder::with_rule(&StandardGomoku).strict().start();

    tx2.make_move((0, 0).into());
    assert_next_err(event_rx, "not your turn to move");
}

fn assert_next_err(event_rx: Receiver<Event>, msg: &str) {
    let actual_msg = event_rx
        .iter()
        .find_map(|e| {
            if let Event::Error(_, _, m) = e {
                Some(m)
            } else {
                None
            }
        })
        .unwrap();
    assert_eq!(actual_msg, msg);
}
