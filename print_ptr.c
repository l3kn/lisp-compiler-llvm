#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#define tag_mask 0b111
#define fixnum_tag 0b000
#define const_tag 0b111

#define true_value 0b111
#define false_value 0b011
#define null_value 0b000

// all scheme values are of type ptr
typedef unsigned long ptr;

extern void print_ptr(ptr x, char* heap) {
  unsigned long tag = x & tag_mask;
  unsigned long value = x >> 3;
  if (tag == fixnum_tag) {
    printf("%d", ((int)value));
  } else if (tag == const_tag) {
    if (value == true_value) {
      printf("#t");
    } else if (value == false_value) {
      printf("#f");
    } else if (value == null_value) {
      printf("()");
    } else {
      printf("Unknown constant: %08x", value);
    }
  } else {
    printf("#<unknown 0x%08x>", x);
  }
}

extern void puts_ptr(ptr x, char* heap) {
  print_ptr(x, heap);
  printf("\n");
}