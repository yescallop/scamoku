use std::thread;

use scamoku::{
    game::{self, Handle},
    rule::Variant,
};

use tokio::runtime::Builder as RtBuilder;

fn main() {
    let Handle {
        mut event_rx,
        cmd_tx,
        ctrl,
    } = game::Builder::with_rule(&Variant::StandardGomoku)
        .strict(true)
        .build();

    thread::spawn(|| {
        RtBuilder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(ctrl.start());
    });

    for _ in 0..2 {
        cmd_tx.pass();
    }

    while let Some(event) = event_rx.blocking_recv() {
        println!("{:?}", event);
    }
}
