#include "chibicc.h"
#include <stdlib.h>

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

static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    // Variable-length array, which is always local.
    if (node->var->ty->kind == TY_VLA) {
      println("  mov s3 %s", node->var->name);
      return;
    }

    // Local variable
    if (node->var->is_local) {
      println("  cpy s3 $l.%s", node->var->name);
      return;
    }

    // Global variable or fucntion
    println("  mov s3 $%s", node->var->name);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    error("STRUCT(gen_addr)) unsupported for now");
    return;
  case ND_FUNCALL:
    if (node->ret_buffer) {
      gen_expr(node);
      return;
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    error_tok(node->tok, "Wyd in the fuck!? That is not supported (gen_addr 1)");
    break;
  case ND_VLA_PTR:
    error("VLA_PRT (gen_addr) unsupported for now");
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void gen_expr(Node *node) {
  println("  ;;.loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM: {
    if (node->val < 0) {
      warn_tok(node->tok, "ND_NUM expression is negative. Using zero instead.\n    HINT: Instead of using [var]++ use asm(\"inc l.[var]\") for better performance");
      node->val = 0;
    }
    println("  mov s3 #%u", node->val);
    return;
  }
  case ND_NEG:
    error("NEG unsupported");
    return;
  case ND_VAR:
    if (node->var->is_local)
      println("  cpy s3 l.%s", node->var->name);
    else
      println("  cpy s3 %s", node->var->name);;
    return;
  case ND_MEMBER: {
    error("STRUCT unsupported for now");
    return;
  }
  case ND_DEREF:
    gen_expr(node->lhs);
    println("  cpy s2 *s3");
    println("  mov s3 s2");
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    if (node->lhs->kind == ND_VAR && node->lhs->var->ty->kind != TY_VLA) {
      gen_expr(node->rhs);
      // Local variable
      if (node->lhs->var->is_local) {
        println("  mov l.%s s3", node->lhs->var->name);
      }
      else // Global variable or fucntion
        println("  mov %s s3", node->lhs->var->name);

      return;
    }
    gen_addr(node->lhs);
    println("  mov s6 s3");
    gen_expr(node->rhs);
    println("  cpy *s6 s3");
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    //warn_tok(node->tok, "casting is ignored");
    gen_expr(node->lhs);
    /* cast(node->lhs->ty, node->ty); */
    return;
  case ND_MEMZERO:
    // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
    error_tok(node->tok, "not implemented (memzero)");
    return;
  case ND_COND: {
    error("one line condition unsupported");
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    int c = count();
    println("  jz s3 .L.set_true.%d", c);
    println("  movz s3");
    println("  jmp .L.end.%d", c);
    println("  .L.set_true.%d:", c);
    println("  inc s3");
    println("  .L.end.%d:", c);
    return;
  case ND_BITNOT:
    error("BITNOT unsupported");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    println("  jz s3 .L.false.%d", c);
    gen_expr(node->rhs);
    println("  jz s3 .L.false.%d", c);
    println("  mov s3 #1");
    println("  jmp .L.end.%d", c);
    println(".L.false.%d:", c);
    println("  movz s3");
    println(".L.end.%d:", c);
    return;
  }
    case ND_LOGOR: {
      int c = count();
      gen_expr(node->lhs);
      println("  jnz s3 .L.true.%d", c);
      gen_expr(node->rhs);
      println("  jnz s3 .L.true.%d", c);
      println("  movz s3");
      println("  jmp .L.end.%d", c);
      println(".L.true.%d:", c);
      println("  mov s3 #1");
      println(".L.end.%d:", c);
      return;
    }
    case ND_FUNCALL: {
      println("  ;; funcall");
      warn_tok(node->tok, "no funcall supported atm");
      /* if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) { */
      /*   gen_expr(node->args); */
      /*   println("  mov %%rax, %%rdi"); */
      /*   builtin_alloca(); */
      /*   return; */
      /* } */

      /* int stack_args = push_args(node); */
      /* gen_expr(node->lhs); */

      /* int gp = 0, fp = 0; */

      /* // If the return type is a large struct/union, the caller passes */
      /* // a pointer to a buffer as if it were the first argument. */
      /* if (node->ret_buffer && node->ty->size > 16) */
      /*   pop(argreg64[gp++]); */

      /* for (Node *arg = node->args; arg; arg = arg->next) { */
      /*   Type *ty = arg->ty; */

      /*   switch (ty->kind) { */
      /*   case TY_STRUCT: */
      /*   case TY_UNION: */
      /*     if (ty->size > 16) */
      /*       continue; */

      /*     bool fp1 = has_flonum1(ty); */
      /*     bool fp2 = has_flonum2(ty); */

      /*     if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) { */
      /*       if (fp1) */
      /*         popf(fp++); */
      /*       else */
      /*         pop(argreg64[gp++]); */

      /*       if (ty->size > 8) { */
      /*         if (fp2) */
      /*           popf(fp++); */
      /*         else */
      /*           pop(argreg64[gp++]); */
      /*       } */
      /*     } */
      /*     break; */
      /*   case TY_FLOAT: */
      /*   case TY_DOUBLE: */
      /*     if (fp < FP_MAX) */
      /*       popf(fp++); */
      /*     break; */
      /*   case TY_LDOUBLE: */
      /*     break; */
      /*   default: */
      /*     if (gp < GP_MAX) */
      /*       pop(argreg64[gp++]); */
      /*   } */
      /* } */

      /* println("  mov %%rax, %%r10"); */
      /* println("  mov $%d, %%rax", fp); */
      /* println("  call *%%r10"); */
      /* println("  add $%d, %%rsp", stack_args * 8); */

      /* depth -= stack_args; */

      /* // It looks like the most significant 48 or 56 bits in RAX may */
      /* // contain garbage if a function return type is short or bool/char, */
      /* // respectively. We clear the upper bits here. */
      /* switch (node->ty->kind) { */
      /* case TY_BOOL: */
      /*   println("  movzx %%al, %%eax"); */
      /*   return; */
      /* case TY_CHAR: */
      /*   if (node->ty->is_unsigned) */
      /*     println("  movzbl %%al, %%eax"); */
      /*   else */
      /*     println("  movsbl %%al, %%eax"); */
      /*   return; */
      /* case TY_SHORT: */
      /*   if (node->ty->is_unsigned) */
      /*     println("  movzwl %%ax, %%eax"); */
      /*   else */
      /*     println("  movswl %%ax, %%eax"); */
      /*   return; */
      /* } */

      /* // If the return type is a small struct, a value is returned */
      /* // using up to two registers. */
      /* if (node->ret_buffer && node->ty->size <= 16) { */
      /*   copy_ret_buffer(node->ret_buffer); */
      /*   println("  lea %d(%%rbp), %%rax", node->ret_buffer->offset); */
      /* } */

      return;
    }
    case ND_LABEL_VAL:
      println("  cpy $%s s3", node->unique_label);
      return;
    case ND_CAS: {
      error("CAS unsupported");
      return;
    }
    case ND_EXCH: {
      error("EXCH unsupported");
      return;
    }
  }

  switch (node->kind) {
    case ND_ADD:
      gen_expr(node->lhs);
      println("  mov s2 s3");
      gen_expr(node->rhs);
      println("  add s3 s2");
      return;
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
      warn_tok(node->tok, "Arithmetic Operation unsupported atm");
      return;
  case ND_BITAND:
    error("BITAND unsupported");
    return;
  case ND_BITOR:
    error("BITOR unsupported");
    return;
  case ND_BITXOR:
    error("BITXOR unsupported");
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    warn_tok(node->tok, "Operation unsupported atm");
    return;
  case ND_SHL:
    error("SHL unsupported");
    return;
  case ND_SHR:
    error("SHR unsupported");
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid expression");
}


static void gen_stmt(Node *node) {
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    println("  jz  s3 .L.else.%d", c);
    gen_stmt(node->then);
    println("  jmp .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els)
      gen_stmt(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      println("  jz s3 %s", node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_expr(node->inc);
    println("  jmp .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    println("  jnz s3 .L.begin.%d", c);
    println("%s:", node->brk_label);
    warn_tok(node->tok, "Operation unsupported atm");
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
    /* println("$%s:", node->brk_label); */
    warn_tok(node->tok, "Operation unsupported atm");
    return;
  case ND_CASE:
    println("$%s:", node->label);
    gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    println("  jmp %s", node->unique_label);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    println("  jmp *s3");
    return;
  case ND_LABEL:
    println("$%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN:
    if (node->lhs)
      gen_expr(node->lhs);

    println("  mov r0 s3");
    println("  ret");
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

void print_escaped_data(Obj* data) {
  if (data->ty->base && data->ty->base->kind == TY_CHAR) {
    if (data->init_data != NULL && data->init_data[0] != 0)
      println("  #\"%s\"", data->init_data);
    else
      println("  0");
    return;
  }
  else if (data->ty->base && is_numeric(data->ty->base)) {
    /* for (int i = 0; i < data->ty->array_len; i++) */
    /*   println("  0"); */
    println("  0");
    return;
  }
  println("  0");
  warn_tok(data->tok, "unhandled case in print_escaped_data; defaulting to 0");
}

static void emit_globals(Obj *prog) {
  for (Obj *var = prog; var && !pure_mur; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    println("$%s:", var->name);
    
    if (var->init_data && (var->ty->size == 1 || (is_numeric(var->ty)))) {
      println("  %u", unsign(var->init_data));
    }
    else if (var->init_data || var->ty->base) {
      print_escaped_data(var);
    }
    else {
      println("  0");
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
    current_fn = fn;

    // locals
    for (Obj *var = fn->locals; var && !pure_mur; var = var->next) {
      if (var->name[0] == 0) {
        var->name = calloc(1, snprintf(NULL, 0, ".OFFSET.%d", var->offset));
        sprintf(var->name, "V.NONAME.%d", count());
      }
      println("$l.%s:", var->name);
      if (var->ty->size == 1 || (is_numeric(var->ty))) {
        println("  0");
        continue;
      }
      else if (var->ty->size > 1) {
        print_escaped_data(var);
        continue;
      }
      else {
        println("  0");
        continue;
      }

      error_tok(var->tok, "locals setup unreachable reached");
    }

    // Emit code
    if (strcmp(fn->name, "_start") == 0)
        println("%s:", fn->name); // Entry point can't be taken as reference
    else
        println("$%s:", fn->name);
    // mov args
    int reg = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      println("  mov l.%s r%d", var->name, reg);
    }
    // clear non-static locals
    for (Obj *var = fn->locals; var && !pure_mur; var = var->next) {
      if (!var->is_static)
        println("  movz l.%s", var->name);
    }

    gen_stmt(fn->body);

    assert(depth == 0);

    //Epilogue
    if (strcmp(fn->name, "main") == 0)
      println("  movz r0"); // C standard say so, ig?

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

