use std::env;
#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input : i64, memory : *mut i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub fn snek_error(errcode : i64) {
  eprintln!("An error occurred {}", errcode);
  std::process::exit(1);
}

fn structural_eq(val1: i64, val2: i64, seen : &mut Vec<(i64, i64)>) -> bool{
  if val1 != val2 {
    // Only if they are addresses then we need to check
    if (val1 != 1 && val1 & 1 == 1) && (val2 != 1 && val2 & 1 == 1){
      if seen.contains(&(val1, val2))  { return true }
      seen.push((val1, val2));
      let addr1 = (val1 - 1) as *const i64;
      let addr2 = (val2 - 1) as *const i64;
      let size1 = unsafe { *addr1 } / 2;
      let size2 = unsafe { *addr2 } / 2;
      if size1 != size2{
        return false;
      }
      for i in 0..size1 {
        let elt1 = unsafe { *addr1.offset(1 + i as isize) };
        let elt2 = unsafe { *addr2.offset(1 + i as isize) };
        if structural_eq(elt1, elt2, seen) == false{
          return false;
        }
      }
      seen.pop();
      return true;

    }
    else{
      return false;
    }
  }
  else{
    return true;
  }
}
#[no_mangle]
#[export_name = "\x01structural_check"]
fn structual_check(val1: i64, val2: i64) -> i32{
  let mut seen = Vec::<(i64, i64)>::new();
  if val1 == 7 || val1 == 3 || val1 % 2 == 0 || val1 == 1{
    //println!("val1 is {}", val1);
    snek_error(900)
  }
  if val2 == 7 || val2 == 3 || val2 % 2 == 0 || val2 == 1{
    //println!("val2 is {}", val2);
    snek_error(900)
  }
  if structural_eq(val1, val2, &mut seen) == true{
    return 7;
  }
  return 3;
}

// let's change snek_str to print ... when it visits a cyclic value
fn snek_str(val : i64, seen : &mut Vec<i64>) -> String {
  if val == 7 { "true".to_string() }
  else if val == 3 { "false".to_string() }
  else if val % 2 == 0 { format!("{}", val >> 1) }
  else if val == 1 { "nil".to_string() }
  else if val & 1 == 1 {
    if seen.contains(&val)  { return "(pair/vector <cyclic>)".to_string() }
    seen.push(val);
    let addr = (val - 1) as *const i64;
    let size = unsafe { *addr } / 2;
    let mut vals = String::new();
    for i in 0..size {
      let elt = unsafe { *addr.offset(1 + i as isize) };
      vals += &format!(" {}", snek_str(elt, seen));
    }
    let result = format!("(array{})", vals);
    // let fst = unsafe { *addr };
    // let snd = unsafe { *addr.offset(1) };
    // let result = format!("(pair {} {})", snek_str(fst, seen), snek_str(snd, seen));
    seen.pop();
    return result;
  }
  else {
    format!("Unknown value: {}", val)
  }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : i64) -> i64 {
  let mut seen = Vec::<i64>::new();
  println!("{}", snek_str(val, &mut seen));
  return val;
}
fn parse_arg(v : &Vec<String>) -> i64 {
  if v.len() < 2 { return 1 }
  let s = &v[1];
  if s == "true" { 3 }
  else if s == "false" { 1 }
  else { s.parse::<i64>().unwrap() << 1 }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_arg(&args);

    let mut memory = Vec::<i64>::with_capacity(1000000);
    let buffer :*mut i64 = memory.as_mut_ptr();
    
    let i : i64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i);
}


