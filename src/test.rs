// use std::cell::RefCell;
// use std::rc::Rc;
// fn main() {
//     let r1 = Rc::new(RefCell::new(42));
//     let r2 = Rc::clone(&r1);
//     let mut m = r1.try_borrow_mut().unwrap();
//     *m = 700;
//     println!("{:?}", r1);
//     println!("{:?}", *r2);
//     println!("{}", m);
// }
