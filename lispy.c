#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>

#include "mpc.h"
// LEFT OFF AT chapter 11
// TODO:
// - implement % operator
// - add a decimal type using 'double'
// - add builtin functions:
//	+ `cons`: takes a value and a QExpr and appends it to the front
//	+ `len`: counts the number of elements in a QExpr
//	+ `init`: returns all but the last element in a QExpr

// built in names
#define LEXIT "exit"
#define LHEAD "head"
#define LTAIL "tail"
#define LLIST "list"
#define LJOIN "join"
#define LEVAL "eval"
#define LDEF  "def"
#define LLAMBDA  "\\"

const int SPECIAL_COUNT = 1;
const char* SPECIAL_FUNCTIONS[SPECIAL_COUNT] = { LEXIT };

/********************************************
 *	Our lisp type and multiple constructors
 ********************************************/


struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

enum { LVAL_NUM, LVAL_ERR, LVAL_SYM,
			 LVAL_FUN, LVAL_SEXPR, LVAL_QEXPR };

enum { LERR_DIV_ZERO, LERR_BAD_NUM, LERR_BAD_OP };

typedef lval*(*lbuiltin)(lenv*, lval*);

struct lval {
	int type;

	// basic
	long num;
	char* err;
	char* sym;

	// function
	lbuiltin builtin;
	lenv* env;
	lval* formals;
	lval* body;
	
	// expression
	int count;
	lval** cell;
};

struct lenv {
	lenv* parent;
	int count;
	char** syms;
	lval** vals;
};

lenv* lenv_copy(lenv* v);
lenv* lenv_new(void);
void lenv_del(lenv* e);

// construct a num value 
lval* lval_num(const long x) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_NUM;
	v->num = x;
	return v;
}

// construct an err value
lval* lval_err(const char* fmt, ...) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_ERR;
	// create the va list and init it
	va_list va;
	va_start(va, fmt);
	// allocate 512 bytes since we don't
	// know the size
	v->err = malloc(512);
	// printf the error string with a max of 511 chars
	vsnprintf(v->err, 511, fmt, va);
	v->err = realloc(v->err, strlen(v->err) + 1);
	//clean up
	va_end(va);
	return v;
}

// construct a symbol
lval* lval_sym(const char* sym) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_SYM;
	v->sym = malloc(strlen(sym) + 1);
	strcpy(v->sym, sym);
	return v;
}

// construct a builtin
lval* lval_builtin(lbuiltin func, char* name) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_FUN;
	v->builtin = func;
	return v;
}

lval* lval_lambda(lval* formals, lval* body) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_FUN;
	v->builtin = NULL;
	v->env = lenv_new();
	v->formals = formals;
	v->body = body;
	return v;
}

// construct an s-expr
lval* lval_sexpr(void) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_SEXPR;
	v->count = 0;
	v->cell = NULL;
	return v;
}

// construct a q-expr
lval* lval_qexpr(void) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_QEXPR;
	v->count = 0;
	v->cell = NULL;
	return v;
}

// destructor for all lvals
void lval_del(lval* v) {
	switch (v->type) {
		// no-op
		case LVAL_NUM: break;
		// free string data
		case LVAL_ERR: free(v->err); break;
		case LVAL_SYM: free(v->sym); break;
		// functions
		case LVAL_FUN:
			if (!v->builtin) {
				lenv_del(v->env);
				lval_del(v->formals);
				lval_del(v->body);
			}
			break;
		// delete child elements
		case LVAL_QEXPR:
		case LVAL_SEXPR:
			for (int i = 0; i < v->count; i++) {
				lval_del(v->cell[i]);
			}
			// free the collection container itself
			free(v->cell);
			break;
	}
	// free the lval itself
	free(v);
}

lval* lval_copy(lval* v) {
	lval* x = malloc(sizeof(lval));
	x->type = v->type;

	switch (v->type) {
		case LVAL_FUN:
			if (v->builtin) {
				x->builtin = v->builtin;
			}
			else {
				x->builtin = NULL;
				x->env = lenv_copy(v->env);
				x->formals = lval_copy(v->formals);
				x->body = lval_copy(v->body);
			}
			break;
		case LVAL_NUM: x->num = v->num; break;

		case LVAL_ERR:
			x->err = malloc(strlen(v->err) + 1);
			strcpy(x->err, v->err);
			break;

		case LVAL_SYM:
			x->sym = malloc(strlen(v->sym) + 1);
			strcpy(x->sym, v->sym);
			break;

		case LVAL_SEXPR:
		case LVAL_QEXPR:
			x->count = v->count;
			x->cell = malloc(sizeof(lval*) * x->count);
			for (int i = 0; i < x->count; i++) {
				x->cell[i] = lval_copy(v->cell[i]);
			}
			break;
	}

	return x;
}

lenv* lenv_new(void) {
	lenv* e = malloc(sizeof(lenv));
	e->parent = NULL;
	e->count = 0;
	e->syms = NULL;
	e->vals = NULL;
	return e;
}

void lenv_del(lenv* e) {
	for (int i = 0; i < e->count; i++) {
		free(e->syms[i]);
		lval_del(e->vals[i]);
	}
	free(e->syms);
	free(e->vals);
	free(e);
}

lenv* lenv_copy(lenv* e) {
	lenv* new_env = malloc(sizeof(lenv));
	new_env->parent = e->parent;
	int count = e->count;
	new_env->count = count;
	new_env->syms = malloc(sizeof(char*) * count);
	new_env->vals = malloc(sizeof(lval*) * count);
	for (int i = 0; i < count; i++) {
		new_env->syms[i] = malloc(strlen(e->syms[i] + 1));
		strcpy(new_env->syms[i], e->syms[i]);
		new_env->vals[i] = lval_copy(e->vals[i]);
	}
	return new_env;
}

lval* lenv_get(lenv* e, lval* k) {
	for (int i = 0; i < e->count; i++) {
		if (strcmp(e->syms[i], k->sym) == 0) {
			return lval_copy(e->vals[i]);
		}
	}

	// check parent
	if (e->parent) {
		return lenv_get(e->parent, k);
	}
	return lval_err("unbound symbol '%s'!", k->sym);
}

char* lenv_rget(lenv* e, lval* v) {
	for (int i = 0; i < e->count; i++) {
		lval* val = e->vals[i];
		if (val->type == LVAL_FUN && val->builtin == v->builtin) {
			return e->syms[i];
		}
	}
	return NULL;
}

void lenv_put(lenv* e, lval* k, lval* v) {
	// find and replace,
	// or allocate new space for new symbol

	for (int i = 0; i < e->count; i++) {
		if (strcmp(e->syms[i], k->sym) == 0) {
			lval_del(e->vals[i]);
			e->vals[i] = lval_copy(v);
			return;
		}
	}

	int newCount = e->count = e->count + 1;
	e->vals = realloc(e->vals, sizeof(lval*) * newCount);
	e->syms = realloc(e->syms, sizeof(char*) * newCount);
	e->vals[newCount - 1] = lval_copy(v);
	e->syms[newCount - 1] = malloc(strlen(k->sym) + 1);
	strcpy(e->syms[newCount - 1], k->sym);
}

void lenv_def(lenv* e, lval* k, lval* v) {
	// put the definition in the outermost env
	while (e->parent) {
		e = e->parent;
	}
	lenv_put(e, k, v);
}

/********************************************
 *	Read functions
 ********************************************/

lval* lval_add(lval* v, lval* x) {
	v->count++;
	v->cell = realloc(v->cell, sizeof(lval*) * v->count);
	v->cell[v->count - 1] = x;
	return v;
}

lval* lval_read_num(mpc_ast_t* t) {
	errno = 0;
	long x = strtol(t->contents, NULL, 10);
	return errno != ERANGE ? lval_num(x) : lval_err("Invalid number");
}

lval* lval_read(mpc_ast_t* t) {
	// terminals
	if (strstr(t->tag, "number")) { return lval_read_num(t); }
	if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }
	// root or s-expr
	lval* x = NULL;
	if (strcmp(t->tag, ">") == 0 || strstr(t->tag, "sexpr")) {
		x = lval_sexpr();
	}
	if (strstr(t->tag, "qexpr")) {
		x = lval_qexpr();
	}
	for (int i = 0; i < t->children_num; i++) {
		if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
		if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
		if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
		if (strcmp(t->children[i]->contents, "}") == 0) { continue; }
		if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
		x = lval_add(x, lval_read(t->children[i]));

	}
	return x;
}

/********************************************
 *	Evaluation functions
 ********************************************/
char* ltype_name(int t);
lval* lval_eval(lenv* e, lval* v);
lval* lval_take(lval* v, int i);
lval* lval_pop(lval* v, int i);
lval* lval_join(lval* x, lval* y);
int lval_is_special(lenv* env, lval* val);
lval* builtin_eval(lenv* e, lval* val);

lval* lval_call(lenv* e, lval* f, lval* v) {
	if (f->builtin) {
		return f->builtin(e, v);
	}

	int given = v->count;
	int total = f->formals->count;

	// partially apply until done
	while (v->count) {
		if (f->formals->count == 0) {
			lval_del(v);
			return lval_err(
				"Function passed too many arguments. "
				"Got %i, Expected %i.", given, total);
		}

		// pop the symbol and argument
		lval* sym = lval_pop(f->formals, 0);
		lval* val = lval_pop(v, 0);

		// put it in the local env
		lenv_put(f->env, sym, val);

		// clean up
		lval_del(sym);
		lval_del(val);
	}

	lval_del(v);

	if (f->formals->count == 0) {
		// if everything is bound then evaluate
		f->env->parent = e;
		return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
	}
	else {
		// otherwise return partially applied function
		return lval_copy(f);
	}
}

lval* lval_eval_sexpr(lenv* env, lval* val) {
	// evaluate children
	for (int i = 0; i < val->count; i++) {
		val->cell[i] = lval_eval(env, val->cell[i]);
	}

	// check for errors
	for (int i = 0; i < val->count; i++) {
		if (val->cell[i]->type == LVAL_ERR) { 
			return lval_take(val, i); 
		}
	}

	// empty expr
	if (val->count == 0) {
		return lval_err("empty expression!");
	}

	// single expr
	int is_single = val->count == 1;
	if (is_single) {
		// special treatment for zero arg functions
		if (lval_is_special(env, val->cell[0])) {
			lval* exit_fn = lval_pop(val, 0);
			lval* result = lval_call(env, exit_fn, val);
			lval_del(exit_fn);
			return result;
		}
		// otherwise remove and print function name
		return lval_take(val, 0);
	}

	// ensure first elem is function
	lval* f = lval_pop(val, 0);
	if (f->type != LVAL_FUN) {
		// uh oh - clean up!
		lval* err = lval_err(
			"S-Expression starts with incorrect type. "
			"Got %s, Expected %s.",
			ltype_name(f->type), ltype_name(LVAL_FUN));
		lval_del(val);
		lval_del(f);
		return err;
	}

	// call builtin operator eval
	lval* result = lval_call(env, f, val);
	lval_del(f);
	return result;
}

int lval_is_special(lenv* env, lval* val) {
	int is_function = val->type == LVAL_FUN;
	if (!is_function) return 0;
		// special treatment for zero arg functions
	for (int i = 0; i < SPECIAL_COUNT; i++) {
		if (strcmp(SPECIAL_FUNCTIONS[i], lenv_rget(env, val)) == 0) {
			return 1;
		}
	}
	return 0;
}

lval* lval_eval(lenv* e, lval* v) {
	if (v->type == LVAL_SYM) {
		lval* x = lenv_get(e, v);
		lval_del(v);
		return x;
	}
	// evaluate S-expr
	if (v->type == LVAL_SEXPR) {
		return lval_eval_sexpr(e, v);
	}
	// all other remain same
	return v;
}

lval* lval_pop(lval* val, int i) {
	lval* x = val->cell[i];
	// shift memory after item 'i' over the top
	memmove(&val->cell[i],
			&val->cell[i + 1],
			sizeof(lval*) * (val->count - 1));
	val->count--;
	// realloc memory used
	val->cell = realloc(val->cell, sizeof(lval*) * val->count);
	return x;
}

lval* lval_take(lval* val, int i) {
	lval* x = lval_pop(val, i);
	lval_del(val);
	return x;
}

char* ltype_name(int t) {
	switch(t) {
		case LVAL_FUN: return "Function";
		case LVAL_NUM: return "Number";
		case LVAL_ERR: return "Error";
		case LVAL_SYM: return "Symbol";
		case LVAL_SEXPR: return "S-Expression";
		case LVAL_QEXPR: return "Q-Expression";
		default: return "Unknown";
	}
}

/********************************************
 *	Builtin functions
 ********************************************/

#define LASSERT(args, cond, fmt, ...) \
 	if (!(cond)) { \
 		lval* err = lval_err(fmt, ##__VA_ARGS__); \
 		lval_del(args); \
 		return err; \
 	}

#define LASSERT_TYPE(func, args, index, expected_type) \
 	LASSERT(args, args->cell[index]->type == expected_type, \
		"Function '%s' passed incorrect type for argument %i. " \
		"Got %s, Expected %s", \
		func, index, \
		ltype_name(args->cell[index]->type), ltype_name(expected_type));

#define LASSERT_COUNT(func, args, num) \
 	LASSERT(args, args->count == num, \
 		"Function '%s' passed incorrect number of arguments. " \
 		"Got %i, Expected %i", \
 		func, args->count, num);

#define LASSERT_NOT_EMPTY(func, args, index) \
 	LASSERT(args, args->cell[index]->count != 0, \
 		"Function '%s' passed {} for argument %i.", \
 		func, index);

lval* lval_join(lval* x, lval* y) {
	while (y->count) {
		x = lval_add(x, lval_pop(y, 0));
	}
	lval_del(y);
	return x;
}

lval* lval_exit() {
	return lval_sym(LEXIT);
}

lval* builtin_op(lval* val, char* op) {
	for (int i = 0; i < val->count; i++) {
		LASSERT_TYPE(op, val, i, LVAL_NUM);
	}

	lval* x = lval_pop(val, 0);
	// for substraction and no args apply unary negation
	if (strcmp("-", op) == 0 && val->count == 0) {
		x->num = -x->num;
	}

	// apply for rest of elems
	while (val->count > 0) {
		lval* y = lval_pop(val, 0);

		if (strcmp("+", op) == 0) { x->num += y->num; }
		if (strcmp("-", op) == 0) { x->num -= y->num; }
		if (strcmp("*", op) == 0) { x->num *= y->num; }
		if (strcmp("/", op) == 0) { 
			if (y->num == 0) {
				lval_del(x);
				lval_del(y);
				x = lval_err("Division by zero!");
				break;
			}
			x->num /= y->num;
		}

		lval_del(y);
	}

	lval_del(val);
	return x;
}

lval* builtin_add(lenv* e, lval* a) {
	return builtin_op(a, "+");
}

lval* builtin_sub(lenv* e, lval* a) {
	return builtin_op(a, "-");
}

lval* builtin_mul(lenv* e, lval* a) {
	return builtin_op(a, "*");
}

lval* builtin_div(lenv* e, lval* a) {
	return builtin_op(a, "/");
}

lval* builtin_head(lenv* e, lval* val) {
	LASSERT_COUNT(LHEAD, val, 1);
	LASSERT_NOT_EMPTY(LHEAD, val, 0);
	LASSERT_TYPE(LHEAD, val, 0, LVAL_QEXPR);
	
	lval* v = lval_take(val, 0);
	while (v->count > 1) {
		lval_del(lval_pop(v, 1));
	}
	return v;
}

lval* builtin_tail(lenv* e, lval* val) {
	LASSERT_COUNT(LTAIL, val, 1);
	LASSERT_TYPE(LTAIL, val, 0, LVAL_QEXPR);
	LASSERT_NOT_EMPTY(LTAIL, val, 0);
	
	lval* v = lval_take(val, 0);
	lval_del(lval_pop(v, 0));
	return v;
}

lval* builtin_list(lenv* e, lval* val) {
	val->type = LVAL_QEXPR;
	return val;
}

lval* builtin_eval(lenv* e, lval* val) {
	LASSERT_COUNT(LEVAL, val, 1);
	LASSERT_TYPE(LEVAL, val, 0 , LVAL_QEXPR);

	lval* x = lval_take(val, 0);
	x->type = LVAL_SEXPR;
	return lval_eval(e, x);
}

lval* builtin_join(lenv* e, lval* val) {
	for (int i = 0; i < val->count; i++) {
		LASSERT_TYPE(LJOIN, val, i, LVAL_QEXPR);
	}

	lval* x = lval_pop(val, 0);
	while (val->count) {
		x = lval_join(x, lval_pop(val, 0));
	}
	lval_del(val);
	return x;
}

lval* builtin_var(lenv* e, lval* v, char* func) {
	LASSERT_TYPE(func, v, 0, LVAL_QEXPR);

	// first arg is symbol list
	lval* syms = v->cell[0];
	// all elements of first list are symbols
	for (int i = 0; i < syms->count; i++) {
		LASSERT(v, syms->cell[i]->type == LVAL_SYM,
			"Function '%s' cannot define a non-symbol. "
			"Got %s, Expected %s.",
			func,
			ltype_name(syms->cell[i]->type),
			ltype_name(LVAL_SYM));
	}
	// check correct number of symbols and values
	LASSERT(v, syms->count == v->count - 1,
		"Function '%s' cannot define incorrect number of values to symbols. "
		"Got %i, Expected %i.",
		func, syms->count, v->count - 1);

	// assign copies of values to symbols
	for (int i = 0; i < syms->count; i++) {
		// if 'def' then define globally
		if (strcmp(func, LDEF) == 0) {
			lenv_def(e, syms->cell[i], v->cell[i + 1]);
		}
		// otherwise put in local env
		if (strcmp(func, "=") == 0) {
			lenv_put(e, syms->cell[i], v->cell[i + 1]);
		}
	}

	lval_del(v);
	return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* v) {
	return builtin_var(e, v, LDEF);
}

lval* builtin_put(lenv* e, lval* v) {
	return builtin_var(e, v, "=");
}

lval* builtin_exit(lenv* e, lval* val) {
	LASSERT_COUNT(LEXIT, val, 0);
	lval_del(val);
	return lval_exit();
}

lval* builtin_lambda(lenv* e, lval* v) {
	LASSERT_COUNT(LLAMBDA, v, 2);
	LASSERT_TYPE(LLAMBDA, v, 0, LVAL_QEXPR);
	LASSERT_TYPE(LLAMBDA, v, 1, LVAL_QEXPR);

	lval* formals = v->cell[0];
	for (int i = 0; i < formals->count; i++) {
		LASSERT(v, formals->cell[i]->type == LVAL_SYM,
			"Cannot define non-symbol. Got %s, Expected %s.",
			ltype_name(formals->cell[i]->type), ltype_name(LVAL_SYM));
	}

	formals = lval_pop(v, 0);
	lval* body = lval_pop(v, 0);
	lval_del(v);

	return lval_lambda(formals, body);
}

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
	lval* k = lval_sym(name);
	lval* v = lval_builtin(func, name);
	lenv_put(e, k, v);
	lval_del(k);
	lval_del(v);
}

void lenv_add_builtins(lenv* e) {
	// special functions
	lenv_add_builtin(e, LDEF, builtin_def);
	lenv_add_builtin(e, "=", builtin_put);
	lenv_add_builtin(e, LEXIT, builtin_exit);
	lenv_add_builtin(e, LLAMBDA, builtin_lambda);
	/* list functions */
	lenv_add_builtin(e, LLIST, builtin_list);
	lenv_add_builtin(e, LHEAD, builtin_head);
	lenv_add_builtin(e, LTAIL, builtin_tail);
	lenv_add_builtin(e, LEVAL, builtin_eval);
	lenv_add_builtin(e, LJOIN, builtin_join);
	/* arithmetic functions */
	lenv_add_builtin(e, "+", builtin_add);
	lenv_add_builtin(e, "-", builtin_sub);
	lenv_add_builtin(e, "*", builtin_mul);
	lenv_add_builtin(e, "/", builtin_div);
}

/********************************************
 *	Print functions
 ********************************************/

void lval_print(lenv* e, lval* v);

void lval_expr_print(lenv* e, lval* v, char open, char close) {
	putchar(open);
	for (int i = 0; i < v->count; i++) {
		lval_print(e, v->cell[i]);
		// don't print separator for last item
		if (i != (v->count - 1)) {
			putchar(' ');
		}
	}
	putchar(close);
}

void lval_print(lenv* e, lval* v) {
	switch (v->type) {
		case LVAL_NUM: 	 printf("%li", v->num); break;
		case LVAL_ERR: 	 printf("Error: %s", v->err); break;
		case LVAL_SYM: 	 printf("%s", v->sym); break;
		case LVAL_FUN:
			if (v->builtin) {
				printf("%s:function", lenv_rget(e, v));
			}
			else {
				printf("\\ ");
				lval_print(e, v->formals);
				putchar(' ');
				lval_print(e, v->body);
				putchar(')');
			}
			break;
		case LVAL_SEXPR: lval_expr_print(e, v, '(', ')'); break;
		case LVAL_QEXPR: lval_expr_print(e, v, '{', '}'); break;
	}
}

void lval_println(lenv* e, lval* v) {
	lval_print(e, v);
	putchar('\n');
}

int number_of_nodes(mpc_ast_t* tree) {
	if (tree->children_num == 0) {
		return 1;
	}
	int n = 1;
	for (int i = 0; i < tree->children_num; ++i) {
		n += number_of_nodes(tree->children[i]);
	}
	return n;
}

int is_exit(lval* val) {
	return (val->type == LVAL_SYM && strcmp(val->sym, LEXIT) == 0);
}


int main(int argc, char** argv) {
	// the parsers
	mpc_parser_t* Number = mpc_new("number");
	mpc_parser_t* Symbol = mpc_new("symbol");
	mpc_parser_t* Sexpr  = mpc_new("sexpr");
	mpc_parser_t* Qexpr  = mpc_new("qexpr");
	mpc_parser_t* Expr   = mpc_new("expr");
	mpc_parser_t* Lispy  = mpc_new("lispy");

	// the language
	mpca_lang(MPCA_LANG_DEFAULT,
		"																											\
		number 	: /-?[0-9]+/ ;																\
		symbol 	: /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;					\
		sexpr 	: '(' <expr>* ')' ; 													\
		qexpr 	: '{' <expr>* '}' ; 													\
		expr 		:  <number> | <symbol> | <sexpr> | <qexpr> ;	\
		lispy 	: /^/ <expr>* /$/ ;														\
		",
		Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

	lenv* env = lenv_new();
	lenv_add_builtins(env);

	puts("Lispy version: 0.0.1");
	puts("Press Ctrl+C to exit or enter 'exit'\n");

	int _exit = 0;
	while (1) {
		char* input = readline("lispy> ");
		add_history(input);
		// attempt to parse the user input
		mpc_result_t r;
		if (mpc_parse("<stdin>", input, Lispy, &r)) {
			lval* x = lval_eval(env, lval_read(r.output));
			_exit = is_exit(x);
			if (!_exit) {
				lval_println(env, x);
			}
			lval_del(x);
			// printf("Num children %d\n", number_of_nodes(r.output));
			mpc_ast_delete(r.output);
		}
		else {
			// .. or else print error
			mpc_err_print(r.error);
			mpc_err_delete(r.error);
		}
		free(input);
		if (_exit) {
			break;
		}
	}

	lenv_del(env);
	mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

	return 0;
}
