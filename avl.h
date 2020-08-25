#ifndef AVL_H
#define AVL_H
/**
 * \brief  An AVL tree implementation.
 * \author Daniel Collins
 * \date   12-18-2018 (US)
 *
 * One of my finer (if more enigmatic) side-projects.
 * This is an AVL tree implementation which is meant to save memory.
 * The nodes are intrusive, and hence, live in the same buffer as their content.
 * This is about the only orthodox trick used to reserve memory.
 *
 * To begin with, it steals two low-order bits from the pointer type.
 * This allows it to store the balance-factor info without needing another word.
 * This is OK with most pointers because pointers are generally aligned.
 * Thus, their lower-order bits are zero'd out anyways.
 *
 * This library also allows one to define a custom pointer type, by redefining
 *  the various macros which are used as methods to interact with the pointer.
 *
 * Thus, some particular #include of this library can, for instance,
 *  use 16-bit shorts as pointers (the lower two bits of which are stolen).
 * These shorts would, naturally, be indicies into a memory pool or something.
 *
 * Thus, not only is the balance info stored in pointers, ensuring that this
 *  library has no size disadvantages over a non-balanced tree,
 *  but also, the pointers can be made *smaller than pointers*.
 *
 * If using the above-mentioned 16-bit indicies scheme, one can still address
 * ~16K elements, but needs only 4 bytes of overhead per element in the tree.
 * This is smaller than the overhead for all hash tables I have ever heard of.
 *
 * Note: Requires that one define AVL_UPTR_T if using the default pointer type.
 *       This is so that we don't depend on the C stdlib for uintptr_t.
 *
 * Pretty nifty!
 *
 * The only flaw left is that it recomputes a balance factor on rotation.
 * This is unnecessary slowness - it could use information about which rotation
 *  it's performing to decide the balance factor statically, instead.
 *
 * ----------
 *
 * This file also incluces a really neat command-line tree formatter/printer.
 * Think the output of the 'tree' command, or something like it.
 * Run the test to see it!
 *
 * ----------
 *
 * A note from when I weaseled out the last bug from this damn thing:
 *     -- Debugged successfully at about 2:15 in the morning. --
 */

/**********************
 ** CLIENT INTERFACE **
 ***********************/

#ifndef AVL_REF_T
#define AVL_REF_T void*

/*
 * It is an error to redefine AVL_PTROF when AVL_REF_T was not defined,
 *  but it is not necessarily an error to define AVL_REF_T but not AVL_DEREF.
 */
#ifdef AVL_PTROF
#error "AVL_REF_T not defined, but AVL_PTROF redefined!"
#else
#define AVL_PTROF(aref) ((struct avl_node*)(aref))
#endif /* AVL_PTROF */

/* Same as with AVL_PTROF */
/**
 * The following are operations on the pointer/block types the user defines:
 *
 * AVL_STOW creates an avl_blk from an avl_ref and a bit.
 * AVL_SREF retrieves the "stowed" reference (of type avl_ref) from an avl_blk.
 * AVL_SBIT retrieves the "stowed" bit from an avl_blk.
 */
#if defined(AVL_STOW) || defined(AVL_SREF) || defined(AVL_SBIT)
#error "AVL_REF_T left default, but AVL_STOW || AVL_SREF || AVL_SBIT redefined!"
#else
#define AVL_STOW(aref, bit) ( (avl_blk ) ((AVL_UPTR_T)(aref) | (bit))          )
#define AVL_SREF(ablk)      ( (avl_ref ) ((AVL_UPTR_T)(ablk) & ~(AVL_UPTR_T)1) )
#define AVL_SBIT(ablk)      ( (unsigned) ((AVL_UPTR_T)(ablk) &              1) )
#endif /* AVL_STOW || AVL_SREF || AVL_SBIT */

#ifndef AVL_UPTR_T
#error "Default avl_ref definition choosen, but w/out a uintptr_t specified!"
#endif /* !AVL_UPTR_T */
#endif /* !AVL_REF_T  */

#ifndef AVL_BLK_T
#define AVL_BLK_T AVL_REF_T
#endif /* AVL_BLK_T */

/*
 * An avl_blk holds a single bit for balance factor information, and an avl_ref.
 */
typedef AVL_REF_T   avl_ref;
typedef AVL_BLK_T   avl_blk;
typedef const void* avl_key;

/**
 * Intrusive node superclass; Node types must subclass ``struct avl_node.''
 */
struct avl_node { avl_blk children[2]; };
enum   avl_dirs { AVL_LEFT, AVL_RIGHT  };

typedef unsigned(*avl_cmp)(const void* left_key, const void* right_key);
typedef struct {
	avl_blk  root;
	avl_cmp  ltcmp;
	unsigned k_off; /* Offset of the key in each node. */
} avl_tree;

/**
 * A record of nodes visited in a search, excluding the query result proper.
 *
 * For some node with an index ``i,'' in the ``path'' array,
 *  the very same node has an entry in the ``dirs'' array, which caches
 *  the direction the ``i+1th'' node is relative to this ``ith'' node.
 * ``ncnt'' defines the number of such entries.
 * ``path'' contains a path to the found child, all the way from the root,
 *  up to but not including, the found node itself.
 */
typedef struct {
	avl_blk*      path[32]; /* Parents.                */
	char          dirs[32]; /* Direction of childhood. */
	unsigned long ncnt;     /* Path length.            */
} avl_path;

/*************
 ** HELPERS **
 *************/

/** Convenience macro for setting the bit from a pointer to avl_blk. */
#define AVL_SET_BIT(bptr, bit) (*(bptr) = AVL_STOW(AVL_SREF(*(bptr)), (bit)))

/**
 * Sets the right child to hold a 1 if the avl_blk bit is r - l == 1.
 * Sets the left child to hold a 1 there if r - l == -1.
 *
 * Note: In the future, one could replace the comparisons with some
 *  fast, evil bitwise hackery, e.g., bf >> 1 & 1, because ``bf''
 *  can only be -1, 0, or 1; but this sort of stuff is rather cryptic.
 * Perhaps bitwise operations have already been (ab)used enough in this library.
 * A more obvious function serving the same purpose, should it present itself,
 *  could be a welcome improvement.
 */
#define AVL_SET_BAL(node, bf)\
	(AVL_SET_BIT(&(node)->children[AVL_RIGHT], (bf) == +1),\
	 AVL_SET_BIT(&(node)->children[AVL_LEFT ], (bf) == -1))

/**
 * Extracts the balance factor from the stowed bits in the child avl_blks.
 * Returns a value equal to (depth of right subtree - depth of left subtree).
 */
#define AVL_BFACTOR(node)     ( AVL_SBIT((node)->children[AVL_RIGHT])\
                              - AVL_SBIT((node)->children[AVL_LEFT ]))
#define AVL_2NODE(blk)        ( AVL_PTROF(AVL_SREF(blk))         )
#define AVL_CHILD(node, dir)  ( AVL_2NODE((node)->children[dir]) )
#define AVL_KEYOF(tree, node) ( (avl_key) ((const char*)(node)+(tree)->k_off) )
#define AVL_OTHER(side)       ((side) ^ 01)
#define AVL_MAX(lhs, rhs) ((lhs) > (rhs) ? (lhs) : (rhs))
//#define AVL_MIN(lhs, rhs) ((lhs) < (rhs) ? (lhs) : (rhs)) // TODO: Unused
static void avl_single_rotation(avl_blk* old_root_slot, enum avl_dirs which);
static void avl_double_rotation(avl_blk* root_slot,     enum avl_dirs side );
static int  avl_correct_bfactor(avl_blk* node_slot,     enum avl_dirs side );
static void avl_perform_retrace(const avl_path* path);

/**
 * Used between avl_correct_bfactor and avl_perform_retrace, to indicate
 *  which action a parent of a node updated on some side requires.
 * ``avl_correct_bfactor'' returns one of these values to indicate status.
 *
 * Either a node absorbs a balance change, and retracing finishes,
 *  or it requires rotation to reduce its balance factor to be w/in the range,
 *  or it requires neither of the two above but retracing must continue.
 */
enum _avl_rotation_stat { _avl_finished=0, _avl_must_rot, _avl_continue };

/************************
 ** INVARIANT CHECKING **
 ************************/

#ifndef NDEBUG
static unsigned avl_isbst(const avl_tree* T, const struct avl_node* subtree);
static unsigned avl_isavl(const struct avl_node* subtree);
static int      avl_bfact(const struct avl_node* subtree);
static unsigned avl_depth(const struct avl_node* subtree);
#endif /* !NDEBUG */

/****************
 ** OPERATIONS **
 ****************/

/**
 * Result to a search query.
 */
typedef struct {
	avl_blk*      nvec; /* Pointer to the ``children'' field of a found node. */
	enum avl_dirs side; /* The side found in the above.                       */
} avl_query_result;

/**
 * Create a new AVL tree.
 */
static avl_tree avl_tree_init(avl_cmp lt_compar, unsigned keyoffset)
{
	avl_tree tree;
	tree.root  = (avl_blk)0;
	tree.ltcmp = lt_compar;
	tree.k_off = keyoffset;

	return tree;
}

static void avl_node_init(struct avl_node* n)
{
	n->children[0] = 0, n->children[1] = 0;
}

/**
 * Standard binary search in the tree.
 *
 * @param path is optional, if supplied it will be populated with a trace.
 */
static avl_query_result avl_search(const avl_tree* T, avl_key K, avl_path* path)
{
	avl_path         _dummy;
	avl_query_result result;
	unsigned lcmp, rcmp, side = 0;
	const avl_blk* nvec = &(T->root);

	if (!path) path = &_dummy;
	path->ncnt      = 0;
	while (AVL_SREF(nvec[side]) &&
	       ( lcmp = T->ltcmp(K, AVL_KEYOF(T, AVL_2NODE(nvec[side])))) |
	       ( rcmp = T->ltcmp(AVL_KEYOF(T, AVL_2NODE(nvec[side])), K)) ) {

		/* Record the visited node. */
		path->path[path->ncnt  ] = (avl_blk*)&nvec[side];
		path->dirs[path->ncnt++] = rcmp; /* ``rcmp'' is the next child. */

		/* Advance to the next child. */
		nvec                     = &AVL_2NODE(nvec[side])->children[0];
		side                     = rcmp;
	}

	result.nvec = (avl_blk*)nvec; /* Allow client to mutate the result. */
	result.side =           side;
	return result;
}

static int avl_haskey(avl_query_result r) { return !!AVL_SREF(r.nvec[r.side]); }

/**
 * Deletion:
 * Remove the node comparing equivalent to ``K'', returning it if it exists.
 * Returns the NULL pointer otherwise.
 */
static struct avl_node* avl_remove(avl_tree* T, avl_key K, avl_path path)
	;

/**
 * Insertion:
 * Insert the node ``node''.
 */
static void avl_insert(avl_tree* T, avl_ref node)
{
	avl_path         path;
	avl_query_result res = avl_search(T, AVL_KEYOF(T, AVL_PTROF(node)), &path);
	if (avl_haskey(res)) return;

	res.nvec[res.side] = node;
	avl_perform_retrace(&path);
}

/****************************
 ** HELPER IMPLEMENTATIONS **
 ****************************/

/**
 * ``which'' specifies a left or right rotation.
 */
static void avl_single_rotation(avl_blk* old_root_slot, enum avl_dirs which)
{
	enum   avl_dirs     other = AVL_OTHER(which);
	struct avl_node* old_root = AVL_2NODE(*old_root_slot);
	avl_blk      new_root_blk = old_root->children[other];
	struct avl_node* new_root = AVL_2NODE(new_root_blk);
	int old_root_bf, new_root_bf;

	old_root->children[other] = new_root->children[which];
	new_root->children[which] = *old_root_slot;
	*old_root_slot = AVL_STOW(AVL_SREF(new_root_blk), AVL_SBIT(*old_root_slot));

	/* Set the balance factors. */
	/* TODO: Set bfact in a non-mongoloid way. */
	old_root_bf = avl_bfact(old_root), new_root_bf = avl_bfact(new_root);
	AVL_SET_BAL(old_root, old_root_bf);
	AVL_SET_BAL(new_root, new_root_bf);
}

/**
 * ``side'' defines which side of the tree is unbalanced, and to be corrected.
 */
static void avl_double_rotation(avl_blk* root_slot, enum avl_dirs side)
{
	avl_single_rotation(&(AVL_2NODE(*root_slot)->children[side]), side);
	avl_single_rotation(root_slot, AVL_OTHER(side));
}

static int  avl_correct_bfactor(avl_blk* slot, enum avl_dirs side)
{
	static const int inc_tab[2] = { /* AVL_LEFT: */ -1, /* AVL_RIGHT: */ +1 };
	             int b;
	switch (b=(AVL_BFACTOR(AVL_2NODE(*slot)) + inc_tab[side])) {
	case -2: case +2: /*- ROTATIONS WILL DO UPDATE -*/; return _avl_must_rot;
	case -1: case +1: AVL_SET_BAL(AVL_2NODE(*slot), b); return _avl_continue;
	case 0:           AVL_SET_BAL(AVL_2NODE(*slot), 0); return _avl_finished;
	}
}

static void avl_perform_retrace(const avl_path* p)
{
	avl_blk* blk;
	unsigned n = p->ncnt, side, rot;
	unsigned n_rots = 0;

	/*
	 * Encodes the direction information as integers representing the four cases
	 *  and performs rebalancing for each case.
	 *
	 * The bit in the left position is the direction of the immediate child,
	 *  and the right bit, the lowest order bit, is the direction its child.
	 */
	while (n-- && (blk=p->path[n], rot=avl_correct_bfactor(blk, side=p->dirs[n])))
		if (rot != _avl_must_rot) continue;
		else switch (side<<1 | p->dirs[n+1]) {
		case 0<<1 | 0: avl_single_rotation(blk, AVL_RIGHT); return; /* LL */
		case 1<<1 | 1: avl_single_rotation(blk, AVL_LEFT ); return; /* RR */
		case 1<<1 | 0: avl_double_rotation(blk, AVL_RIGHT); return; /* RL */
		case 0<<1 | 1: avl_double_rotation(blk, AVL_LEFT ); return; /* LR */
	}
}

/*************************
 ** COOL TREE FORMATTER **
 *************************/

struct avl_fmt_ostrm;
static void avl_fmt(const struct avl_node* subtree, struct avl_fmt_ostrm* os);

/*
 * Client must subclass avl_fmt_ostrm and implement these methods.
 *
 * ``print_obj'' is given an AVL tree node, meant to print what it contains.
 * ``print_str'' is given a string, and must print it to the output stream.
 */
typedef void (*avl_fmt_print_str)(struct avl_fmt_ostrm*, const            char*);
typedef void (*avl_fmt_print_obj)(struct avl_fmt_ostrm*, const struct avl_node*);
struct avl_fmt_ostrm
{
	avl_fmt_print_str print_str;
	avl_fmt_print_obj print_obj;
};

/**
 ** PRIVATE
 **/

struct _avl_pfix_node {
	           const char* part;
	struct _avl_pfix_node* next;
};

struct _avl_pfix_chain {
	struct _avl_pfix_node* head;
	struct _avl_pfix_node* tail;
};

static struct _avl_pfix_chain _avl_new_pfix(void)
{
	struct _avl_pfix_chain ch = { /* NULL */ 0, /* NULL */ 0 };
	return ch;
}

static void _avl_print_pfix(const struct _avl_pfix_node* n, struct avl_fmt_ostrm* o)
{
	while (n) o->print_str(o, n->part), n = n->next;
}

static struct _avl_pfix_chain* _avl_append_pfix(struct _avl_pfix_chain* pc,
                                                struct _avl_pfix_node** n)
{
	struct _avl_pfix_node* tmp;

	(*n)->next = /* NULL */ 0;
	if (pc->tail) pc->tail->next = (*n);
	else                pc->head = (*n);

	/* swap old tail into n */
	tmp        = pc->tail;
	pc->tail   = (*n);
	*n         = tmp;
}

static void _avl_pop_pfix(struct _avl_pfix_chain* pc, struct _avl_pfix_node** old)
{
	if (pc->head == pc->tail) pc->head = /* NULL */ 0, pc->tail = /* NULL */ 0;
	else                  (*old)->next = /* NULL */ 0, pc->tail = (*old);
}

static void _avl_fmt_(struct avl_fmt_ostrm* os, struct _avl_pfix_chain* ch,
                      const struct avl_node* node, enum avl_dirs kind, int us)
{
	static const char*          path[2] = { "├── ", "└── " }; /* 0, 1 */
	struct _avl_pfix_node *sel, line    = { "│   " }, space = { "     " };

	_avl_print_pfix(ch->head, os); os->print_str(os, path[kind]);
	if (node) {
		sel = (us) ? &space : &line;

		os->print_obj(os, node); os->print_str(os, "\n");
		_avl_append_pfix(ch, &sel);
		_avl_fmt_(os, ch, AVL_CHILD(node, AVL_RIGHT), 0, 0 /* FAlSE */);
		_avl_fmt_(os, ch, AVL_CHILD(node, AVL_LEFT ), 1, 1 /* TRUE  */);
		_avl_pop_pfix(ch, &sel);
	} else os->print_str(os, "!\n");
}

static void avl_fmt(const struct avl_node* subtree, struct avl_fmt_ostrm* os)
{
	struct _avl_pfix_chain ch = _avl_new_pfix();
	_avl_fmt_(os, &ch, subtree, 1, 1 /* TRUE */);
}

/*
 * What follows are debugging functions, useful for convincing oneself
 *  that the foregoing code does, in fact, implement an AVL tree correctly.
 *
 * I allow recursion here, because the point of these functions is to
 *  *clearly* represent the invariants that distinguish AVL trees and BSTs.
 * Recursion is typically the most natural way of doing this.
 */

#ifndef NDEBUG
static unsigned avl_isavl(const struct avl_node* subtree)
{
	if (!subtree) return 1;

	int bf = avl_bfact(subtree);
	return ( -2 < bf && bf < 2 )	  /* Balance factor must be in range.     */
		&& AVL_BFACTOR(subtree) == bf /* Stored balance factor must be right. */
		&& avl_isavl(AVL_CHILD(subtree, AVL_RIGHT))
		&& avl_isavl(AVL_CHILD(subtree, AVL_LEFT ));
}

static unsigned avl_isbst(const avl_tree* T, const struct avl_node* N)
{
	const struct avl_node *l, *r;
	if (!N) return 1;

	l = AVL_CHILD(N, AVL_LEFT );
	r = AVL_CHILD(N, AVL_RIGHT);
	return (!l || avl_isbst(T, l) && T->ltcmp(AVL_KEYOF(T, l), AVL_KEYOF(T, N)))
	    && (!r || avl_isbst(T, r) && T->ltcmp(AVL_KEYOF(T, N), AVL_KEYOF(T, r)));
}

static int avl_bfact(const struct avl_node* subtree)
{
	if (!subtree) return 0;

	return (int)avl_depth(AVL_CHILD(subtree, AVL_RIGHT))
	     - (int)avl_depth(AVL_CHILD(subtree, AVL_LEFT ));
}

static unsigned avl_depth(const struct avl_node* subtree)
{
	unsigned ld, rd;
	if (!subtree) return 0;
	ld = avl_depth(AVL_CHILD(subtree, AVL_LEFT ));
	rd = avl_depth(AVL_CHILD(subtree, AVL_RIGHT));
	return 1 + AVL_MAX(ld, rd);
}
#endif /* !NDEBUG */
#endif /* AVL_H */
