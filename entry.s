  .intel_syntax noprefix
  .text
  .globl scheme_entry
  .type scheme_entry, @function
scheme_entry:
  # store register contents in ctxt
  mov rcx, rsp
  mov rdx, rbp
  mov rsp, rdi
  mov rbp, rdi
  mov r11, rsi # r11 = heap pointer
  push rcx
  push rdx
  push rbx
  push rsi
  push rdi
  call scheme_body
  pop rdi
  pop rsi
  pop rbx
  pop rbp
  pop rsp
  ret
