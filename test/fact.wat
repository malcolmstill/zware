(module
  (type $t0 (func (param i32) (result i32)))
  (func $fact (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32)
    i32.const 2
    local.get $p0
    i32.ge_s
    if $I0 (result i32)
      local.get $p0
      return
    else
      i32.const 1
      local.set $l2
      i32.const 2
      local.set $l1
      loop $L1
        local.get $l1
        local.get $l2
        i32.mul
        local.set $l2
        local.get $l1
        i32.const 1
        i32.add
        local.set $l1
        local.get $p0
        local.get $l1
        i32.ge_s
        br_if $L1
      end
      local.get $l2
      return
    end)
  (export "fact" (func $fact)))
