(module
  (type $t (func (param i32) (result i32)))
  (func $fib (type $t) (param $n i32) (result i32)
    local.get $n
    i32.const 0
    i32.eq
    if $I0
      i32.const 0
      return
    end
    local.get $n
    i32.const 1
    i32.eq
    if $I1
      i32.const 1
      return
    end
    local.get $n
    i32.const 2
    i32.sub
    call $fib
    local.get $n
    i32.const 1
    i32.sub
    call $fib
    i32.add
    return)
  (export "fib" (func $fib)))