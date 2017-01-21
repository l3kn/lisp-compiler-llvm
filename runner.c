#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#define tag_mask 0b1111
#define fixnum_tag 0b0000
#define const_tag 0b1111

#define true_value 0b11
#define false_value 0b01

// all scheme values are of type ptr
typedef unsigned long ptr;
extern ptr scheme_entry();

static void print_ptr(ptr x, char* heap) {
  unsigned long tag = x & tag_mask;
  unsigned long value = x >> 4;
  if (tag == fixnum_tag) {
    printf("%d", ((int)value));
  } else if (tag == const_tag) {
    if (value == true_value) {
      printf("#t");
    } else if (value == false_value) {
      printf("#f");
    } else {
      printf("Unknown constant: %08x", value);
    }
  } else {
    printf("#<unknown 0x%08x>", x);
  }
}

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;

  // Round to some size that is a multiple of page
  // and bigger or equal to the original size
  int aligned_size = ((size + page - 1) / page) * page;

  // Allocate the aligned size
  // and two additional pages
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED) {
    printf("Failed to allocate");
    exit(EXIT_FAILURE);
  }

  // Protect the first and the last page
  // so the program crashes
  // if the stack over- or underflows
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) {
    printf("Failed to protect the first page");
    exit(EXIT_FAILURE);
  }

  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) {
    printf("Failed to protect the last page");
    exit(EXIT_FAILURE);
  }

  // Add an offset of one page
  // because the first page is protected
  return (p + page);
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;

  status = munmap(p - page, aligned_size + 2 * page);

  if (status != 0) {
    printf("Failed to deallocate");
    exit(EXIT_FAILURE);
  }
}

int main(int argc, char** argv) {
  int stack_size = (4 * 4 * 4096); // 16k 64bit cells
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;

  int heap_size = (4 * 4 * 4096);
  char* heap = allocate_protected_space(heap_size);

  ptr result = scheme_entry(stack_base, heap);

  print_ptr(result, heap);
  printf("\n");

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, heap_size);
  return 0;
}
