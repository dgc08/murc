#include "chibicc.h"

#define GP_MAX 6
#define FP_MAX 8

typedef unsigned int uint;

static FILE *output_file;
static int depth;
static Obj *current_fn;

bool pure_mur;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

__attribute__((format(printf, 1, 2)))
static void print(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
}

__attribute__((format(printf, 1, 2)))
static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

uint unsign(void* p) {
  return *((uint*)p);
}

static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  println("  push r1");
  depth++;
}

static void pop(char *arg) {
  println("  pop %s", arg);
  depth--;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void gen_stmt(Node *node) {
  switch (node->kind) {
  case ND_IF: {
    /* int c = count(); */
    /* gen_expr(node->cond); */
    /* cmp_zero(node->cond->ty); */
    /* println("  je  .L.else.%d", c); */
    /* gen_stmt(node->then); */
    /* println("  jmp .L.end.%d", c); */
    /* println(".L.else.%d:", c); */
    /* if (node->els) */
    /*   gen_stmt(node->els); */
    /* println(".L.end.%d:", c); */
    return;
  }
  case ND_FOR: {
    /* int c = count(); */
    /* if (node->init) */
    /*   gen_stmt(node->init); */
    /* println(".L.begin.%d:", c); */
    /* if (node->cond) { */
    /*   gen_expr(node->cond); */
    /*   cmp_zero(node->cond->ty); */
    /*   println("  je %s", node->brk_label); */
    /* } */
    /* gen_stmt(node->then); */
    /* println("%s:", node->cont_label); */
    /* if (node->inc) */
    /*   gen_expr(node->inc); */
    /* println("  jmp .L.begin.%d", c); */
    /* println("%s:", node->brk_label); */
    return;
  }
  case ND_DO: {
    /* int c = count(); */
    /* println(".L.begin.%d:", c); */
    /* gen_stmt(node->then); */
    /* println("%s:", node->cont_label); */
    /* gen_expr(node->cond); */
    /* cmp_zero(node->cond->ty); */
    /* println("  jne .L.begin.%d", c); */
    /* println("%s:", node->brk_label); */
    return;
  }
  case ND_SWITCH:
    /* gen_expr(node->cond); */

    /* for (Node *n = node->case_next; n; n = n->case_next) { */
    /*   char *ax = (node->cond->ty->size == 8) ? "%rax" : "%eax"; */
    /*   char *di = (node->cond->ty->size == 8) ? "%rdi" : "%edi"; */

    /*   if (n->begin == n->end) { */
    /*     println("  cmp $%ld, %s", n->begin, ax); */
    /*     println("  je %s", n->label); */
    /*     continue; */
    /*   } */

    /*   // [GNU] Case ranges */
    /*   println("  mov %s, %s", ax, di); */
    /*   println("  sub $%ld, %s", n->begin, di); */
    /*   println("  cmp $%ld, %s", n->end - n->begin, di); */
    /*   println("  jbe %s", n->label); */
    /* } */

    /* if (node->default_case) */
    /*   println("  jmp %s", node->default_case->label); */

    /* println("  jmp %s", node->brk_label); */
    /* gen_stmt(node->then); */
    /* println("%s:", node->brk_label); */
    return;
  case ND_CASE:
    /* println("%s:", node->label); */
    /* gen_stmt(node->lhs); */
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    /* println("  jmp %s", node->unique_label); */
    return;
  case ND_GOTO_EXPR:
    /* gen_expr(node->lhs); */
    /* println("  jmp *%%rax"); */
    return;
  case ND_LABEL:
    println("%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN:
    /* if (node->lhs) { */
    /*   gen_expr(node->lhs); */
    /*   Type *ty = node->lhs->ty; */

    /*   switch (ty->kind) { */
    /*   case TY_STRUCT: */
    /*   case TY_UNION: */
    /*     if (ty->size <= 16) */
    /*       copy_struct_reg(); */
    /*     else */
    /*       copy_struct_mem(); */
    /*     break; */
    /*   } */
    /* } */

    println("  ret");
    return;
  case ND_EXPR_STMT:
    /* gen_expr(node->lhs); */
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

void print_escaped_data(Obj* data) {
  if (data->ty->base->kind == TY_CHAR) {
    println("  \"%s\"", data->init_data);
    return;
  }
  for (int i = 0; i < data->ty->size-1; i++) {
    println("  %d", data->init_data[i]);
  }
}

static void emit_globals(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (var->ty->base)
        print("$");
    println("%s:", var->name);
    
    if (var->init_data && (var->ty->size == 1 || (is_numeric(var->ty)))) {
      println("  %u", unsign(var->init_data));
    }
    else if (var->init_data && var->ty->size > 1) {
      print_escaped_data(var);
    }
    else {
      println("%s:", var->name);
      println("0");
    }
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    // Prologue
    println("scope");
    println("%s:", fn->name);
    current_fn = fn;

    // Emit code
    gen_stmt(fn->body);
    assert(depth == 0);

    //Epilogue
    if (strcmp(fn->name, "main") == 0)
      println("  mov r0 #0"); // C standard say so, ig?

    if (pure_mur) // In pure mur, you are responsible for jumping between functions and hlt-ing
      println("  hlt 2");
    else
      println("  ret");

    println("");
  }
}

void codegen(Obj *prog, FILE *out, bool disable_murbin) {
  output_file = out;
  pure_mur = disable_murbin;

  if (!pure_mur)
    println(";!MURBIN");

  File **files = get_input_files();
  for (int i = 0; files[i]; i++)
    println(";; file %d \"%s\"\n", files[i]->file_no, files[i]->name);

  if (!disable_murbin)
    emit_globals(prog);
  println("");
  emit_text(prog);

}

