/*
 * Generates a tree full of random things through normal insertion.
 * Then prints it, and also verifies it satisfies the AVL invariants.
 *
 */

#include <stdint.h> /* uintptr_t */
#define AVL_UPTR_T uintptr_t
#include "../avl.h"

#include  <stdio.h> /* fputs, fprintf */
#include <stddef.h> /* offsetof       */
#include <stdlib.h> /* srand, rand    */

struct integer_node
{
	struct avl_node base;
	       unsigned nval;
};

static unsigned in_lt(const void* lk, const void* rk) {
	return *(unsigned*)lk < *(unsigned*)rk;
}

extern void ino_print_str(struct avl_fmt_ostrm* os, const            char* s);
extern void ino_print_obj(struct avl_fmt_ostrm* os, const struct avl_node* n);
struct int_node_out {
	struct avl_fmt_ostrm base;
	FILE*                strm;
} ino_base = { { ino_print_str, ino_print_obj } };

void ino_print_str(struct avl_fmt_ostrm* os, const char* s) {
	fputs(s, ((struct int_node_out*)os)->strm);
}

void ino_print_obj(struct avl_fmt_ostrm* os, const struct avl_node* n) {
	static const char symbols[] = { '-', '0', '+' };
	fprintf(((struct int_node_out*)os)->strm, "%u (%c)",
	        ((struct integer_node*)n )->nval, symbols[AVL_BFACTOR(n)+1]);
}


struct node_v {
	struct avl_node* vector;
	       unsigned  stride; /* Length of a single element.       */
	       unsigned  length; /* Number of elements in ``vector.'' */
};

static void insert_random(unsigned s, unsigned m, struct node_v* v, avl_tree* t,
                          void(*step)(avl_tree* t))
{
	struct avl_node* cur = v->vector;
	       unsigned    n = v->length;

	srand(s);
	while (n--) {
		((struct integer_node*) cur)->nval = rand() % m;
		avl_node_init(cur), avl_insert(t, cur), step(t);

		cur = (struct avl_node*) (((char*) cur) + v->stride);
	}
}

/**
 * Generates a number from a sequence of characters, like a hash
 *  but with less effort.
 */
unsigned compute_seed(const char* str)
{
	unsigned acc = 0;
	while (*str) acc |= (unsigned char) *str++, acc ^= (acc & 0x3Fffu) << 2;
}

void dump_tree(avl_tree* t)
{

	struct int_node_out    _os = ino_base;
	struct avl_fmt_ostrm* os = (struct avl_fmt_ostrm*)&_os;
	_os.strm                   = stdout;

	avl_fmt(AVL_2NODE(t->root), os);
	puts(avl_isavl(AVL_2NODE(t->root)) ? "Is an avl tree." :"Not an avl tree.");
	puts(avl_isbst(t, AVL_2NODE(t->root)) ? "Is a bst."    :"Not a bst.");
}

void nop(avl_tree* _) {;}

int main(int argc, const char* argv[])
{
	const unsigned      _stride = sizeof(struct integer_node);
	struct integer_node _buffer[4096];

	struct node_v v = { (struct avl_node*)_buffer, _stride, 4096 };
	avl_tree tree   = avl_tree_init(in_lt, offsetof(struct integer_node, nval));

	insert_random(compute_seed(argc > 1 ? argv[0] : ""), 4096u, &v, &tree, nop);
	dump_tree(&tree);
}
