#lang racket

(require parser-tools/lex
         parser-tools/yacc
         "lexer.rkt")

(provide parse)

(parser
 (start ___)
 (end ___)
 (error ___)
 (tokens t et)
 (grammar
  (primary_expression
   ((IDENTIFIER) #f)
   ((constant) #f)
   ((string) #f)
   ((L_PAREN expression R_PAREN) #f)
   ((generic_selection) #f))
  (constant ((I_CONSTANT) #f) ((F_CONSTANT) #f) ((ENUMERATION_CONSTANT) #f))
  (enumeration_constant ((IDENTIFIER) #f))
  (string ((STRING_LITERAL) #f) ((FUNC_NAME) #f))
  (generic_selection
   ((GENERIC L_PAREN assignment_expression COMMA generic_assoc_list R_PAREN) #f))
  (generic_assoc_list
   ((generic_association) #f)
   ((generic_assoc_list COMMA generic_association) #f))
  (generic_association
   ((type_name : assignment_expression) #f)
   ((DEFAULT : assignment_expression) #f))
  (postfix_expression
   ((primary_expression) #f)
   ((postfix_expression L_BRACKET expression R_BRACKET) #f)
   ((postfix_expression L_PAREN R_PAREN) #f)
   ((postfix_expression L_PAREN argument_expression_list R_PAREN) #f)
   ((postfix_expression PERIOD IDENTIFIER) #f)
   ((postfix_expression PTR_OP IDENTIFIER) #f)
   ((postfix_expression INC_OP) #f)
   ((postfix_expression DEC_OP) #f)
   ((L_PAREN type_name R_PAREN L_BRACE initializer_list R_BRACE) #f)
   ((L_PAREN type_name R_PAREN L_BRACE initializer_list COMMA R_BRACE) #f))
  (argument_expression_list
   ((assignment_expression) #f)
   ((argument_expression_list COMMA assignment_expression) #f))
  (unary_expression
   ((postfix_expression) #f)
   ((INC_OP unary_expression) #f)
   ((DEC_OP unary_expression) #f)
   ((unary_operator cast_expression) #f)
   ((SIZEOF unary_expression) #f)
   ((SIZEOF L_PAREN type_name R_PAREN) #f)
   ((ALIGNOF L_PAREN type_name R_PAREN) #f))
  (unary_operator ((&) #f) ((*) #f) ((+) #f) ((-) #f) ((~) #f) ((!) #f))
  (cast_expression
   ((unary_expression) #f)
   ((L_PAREN type_name R_PAREN cast_expression) #f))
  (multiplicative_expression
   ((cast_expression) #f)
   ((multiplicative_expression * cast_expression) #f)
   ((multiplicative_expression / cast_expression) #f)
   ((multiplicative_expression % cast_expression) #f))
  (additive_expression
   ((multiplicative_expression) #f)
   ((additive_expression + multiplicative_expression) #f)
   ((additive_expression - multiplicative_expression) #f))
  (shift_expression
   ((additive_expression) #f)
   ((shift_expression LEFT_OP additive_expression) #f)
   ((shift_expression RIGHT_OP additive_expression) #f))
  (relational_expression
   ((shift_expression) #f)
   ((relational_expression < shift_expression) #f)
   ((relational_expression > shift_expression) #f)
   ((relational_expression LE_OP shift_expression) #f)
   ((relational_expression GE_OP shift_expression) #f))
  (equality_expression
   ((relational_expression) #f)
   ((equality_expression EQ_OP relational_expression) #f)
   ((equality_expression NE_OP relational_expression) #f))
  (and_expression
   ((equality_expression) #f)
   ((and_expression & equality_expression) #f))
  (exclusive_or_expression
   ((and_expression) #f)
   ((exclusive_or_expression ^ and_expression) #f))
  (inclusive_or_expression
   ((exclusive_or_expression) #f)
   ((inclusive_or_expression \| exclusive_or_expression) #f))
  (logical_and_expression
   ((inclusive_or_expression) #f)
   ((logical_and_expression AND_OP inclusive_or_expression) #f))
  (logical_or_expression
   ((logical_and_expression) #f)
   ((logical_or_expression OR_OP logical_and_expression) #f))
  (conditional_expression
   ((logical_or_expression) #f)
   ((logical_or_expression ? expression : conditional_expression) #f))
  (assignment_expression
   ((conditional_expression) #f)
   ((unary_expression assignment_operator assignment_expression) #f))
  (assignment_operator
   ((=) #f)
   ((MUL_ASSIGN) #f)
   ((DIV_ASSIGN) #f)
   ((MOD_ASSIGN) #f)
   ((ADD_ASSIGN) #f)
   ((SUB_ASSIGN) #f)
   ((LEFT_ASSIGN) #f)
   ((RIGHT_ASSIGN) #f)
   ((AND_ASSIGN) #f)
   ((XOR_ASSIGN) #f)
   ((OR_ASSIGN) #f))
  (expression
   ((assignment_expression) #f)
   ((expression COMMA assignment_expression) #f))
  (constant_expression ((conditional_expression) #f))
  (declaration
   ((declaration_specifiers SEMI_COLON) #f)
   ((declaration_specifiers init_declarator_list SEMI_COLON) #f)
   ((static_assert_declaration) #f))
  (declaration_specifiers
   ((storage_class_specifier declaration_specifiers) #f)
   ((storage_class_specifier) #f)
   ((type_specifier declaration_specifiers) #f)
   ((type_specifier) #f)
   ((type_qualifier declaration_specifiers) #f)
   ((type_qualifier) #f)
   ((function_specifier declaration_specifiers) #f)
   ((function_specifier) #f)
   ((alignment_specifier declaration_specifiers) #f)
   ((alignment_specifier) #f))
  (init_declarator_list
   ((init_declarator) #f)
   ((init_declarator_list COMMA init_declarator) #f))
  (init_declarator ((declarator = initializer) #f) ((declarator) #f))
  (storage_class_specifier
   ((TYPEDEF) #f)
   ((EXTERN) #f)
   ((STATIC) #f)
   ((THREAD_LOCAL) #f)
   ((AUTO) #f)
   ((REGISTER) #f))
  (type_specifier
   ((VOID) #f)
   ((CHAR) #f)
   ((SHORT) #f)
   ((INT) #f)
   ((LONG) #f)
   ((FLOAT) #f)
   ((DOUBLE) #f)
   ((SIGNED) #f)
   ((UNSIGNED) #f)
   ((BOOL) #f)
   ((COMPLEX) #f)
   ((IMAGINARY) #f)
   ((atomic_type_specifier) #f)
   ((struct_or_union_specifier) #f)
   ((enum_specifier) #f)
   ((TYPEDEF_NAME) #f))
  (struct_or_union_specifier
   ((struct_or_union L_BRACE struct_declaration_list R_BRACE) #f)
   ((struct_or_union IDENTIFIER L_BRACE struct_declaration_list R_BRACE) #f)
   ((struct_or_union IDENTIFIER) #f))
  (struct_or_union ((STRUCT) #f) ((UNION) #f))
  (struct_declaration_list
   ((struct_declaration) #f)
   ((struct_declaration_list struct_declaration) #f))
  (struct_declaration
   ((specifier_qualifier_list SEMI_COLON) #f)
   ((specifier_qualifier_list struct_declarator_list SEMI_COLON) #f)
   ((static_assert_declaration) #f))
  (specifier_qualifier_list
   ((type_specifier specifier_qualifier_list) #f)
   ((type_specifier) #f)
   ((type_qualifier specifier_qualifier_list) #f)
   ((type_qualifier) #f))
  (struct_declarator_list
   ((struct_declarator) #f)
   ((struct_declarator_list COMMA struct_declarator) #f))
  (struct_declarator
   ((: constant_expression) #f)
   ((declarator : constant_expression) #f)
   ((declarator) #f))
  (enum_specifier
   ((ENUM L_BRACE enumerator_list R_BRACE) #f)
   ((ENUM L_BRACE enumerator_list COMMA R_BRACE) #f)
   ((ENUM IDENTIFIER L_BRACE enumerator_list R_BRACE) #f)
   ((ENUM IDENTIFIER L_BRACE enumerator_list COMMA R_BRACE) #f)
   ((ENUM IDENTIFIER) #f))
  (enumerator_list ((enumerator) #f) ((enumerator_list COMMA enumerator) #f))
  (enumerator
   ((enumeration_constant = constant_expression) #f)
   ((enumeration_constant) #f))
  (atomic_type_specifier ((ATOMIC L_PAREN type_name R_PAREN) #f))
  (type_qualifier
   ((CONST) #f)
   ((RESTRICT) #f)
   ((VOLATILE) #f)
   ((ATOMIC) #f))
  (function_specifier ((INLINE) #f) ((NORETURN) #f))
  (alignment_specifier
   ((ALIGNAS L_PAREN type_name R_PAREN) #f)
   ((ALIGNAS L_PAREN constant_expression R_PAREN) #f))
  (declarator ((pointer direct_declarator) #f) ((direct_declarator) #f))
  (direct_declarator
   ((IDENTIFIER) #f)
   ((L_PAREN declarator R_PAREN) #f)
   ((direct_declarator L_BRACKET R_BRACKET) #f)
   ((direct_declarator L_BRACKET * R_BRACKET) #f)
   ((direct_declarator
     L_BRACKET
     STATIC
     type_qualifier_list
     assignment_expression
     R_BRACKET)
    #f)
   ((direct_declarator L_BRACKET STATIC assignment_expression R_BRACKET) #f)
   ((direct_declarator L_BRACKET type_qualifier_list * R_BRACKET) #f)
   ((direct_declarator
     L_BRACKET
     type_qualifier_list
     STATIC
     assignment_expression
     R_BRACKET)
    #f)
   ((direct_declarator L_BRACKET type_qualifier_list assignment_expression R_BRACKET)
    #f)
   ((direct_declarator L_BRACKET type_qualifier_list R_BRACKET) #f)
   ((direct_declarator L_BRACKET assignment_expression R_BRACKET) #f)
   ((direct_declarator L_PAREN parameter_type_list R_PAREN) #f)
   ((direct_declarator L_PAREN R_PAREN) #f)
   ((direct_declarator L_PAREN identifier_list R_PAREN) #f))
  (pointer
   ((* type_qualifier_list pointer) #f)
   ((* type_qualifier_list) #f)
   ((* pointer) #f)
   ((*) #f))
  (type_qualifier_list
   ((type_qualifier) #f)
   ((type_qualifier_list type_qualifier) #f))
  (parameter_type_list
   ((parameter_list COMMA ELLIPSIS) #f)
   ((parameter_list) #f))
  (parameter_list
   ((parameter_declaration) #f)
   ((parameter_list COMMA parameter_declaration) #f))
  (parameter_declaration
   ((declaration_specifiers declarator) #f)
   ((declaration_specifiers abstract_declarator) #f)
   ((declaration_specifiers) #f))
  (identifier_list ((IDENTIFIER) #f) ((identifier_list COMMA IDENTIFIER) #f))
  (type_name
   ((specifier_qualifier_list abstract_declarator) #f)
   ((specifier_qualifier_list) #f))
  (abstract_declarator
   ((pointer direct_abstract_declarator) #f)
   ((pointer) #f)
   ((direct_abstract_declarator) #f))
  (direct_abstract_declarator
   ((L_PAREN abstract_declarator R_PAREN) #f)
   ((L_BRACKET R_BRACKET) #f)
   ((L_BRACKET * R_BRACKET) #f)
   ((L_BRACKET STATIC type_qualifier_list assignment_expression R_BRACKET) #f)
   ((L_BRACKET STATIC assignment_expression R_BRACKET) #f)
   ((L_BRACKET type_qualifier_list STATIC assignment_expression R_BRACKET) #f)
   ((L_BRACKET type_qualifier_list assignment_expression R_BRACKET) #f)
   ((L_BRACKET type_qualifier_list R_BRACKET) #f)
   ((L_BRACKET assignment_expression R_BRACKET) #f)
   ((direct_abstract_declarator L_BRACKET R_BRACKET) #f)
   ((direct_abstract_declarator L_BRACKET * R_BRACKET) #f)
   ((direct_abstract_declarator
     L_BRACKET
     STATIC
     type_qualifier_list
     assignment_expression
     R_BRACKET)
    #f)
   ((direct_abstract_declarator L_BRACKET STATIC assignment_expression R_BRACKET) #f)
   ((direct_abstract_declarator
     L_BRACKET
     type_qualifier_list
     assignment_expression
     R_BRACKET)
    #f)
   ((direct_abstract_declarator
     L_BRACKET
     type_qualifier_list
     STATIC
     assignment_expression
     R_BRACKET)
    #f)
   ((direct_abstract_declarator L_BRACKET type_qualifier_list R_BRACKET) #f)
   ((direct_abstract_declarator L_BRACKET assignment_expression R_BRACKET) #f)
   ((L_PAREN R_PAREN) #f)
   ((L_PAREN parameter_type_list R_PAREN) #f)
   ((direct_abstract_declarator L_PAREN R_PAREN) #f)
   ((direct_abstract_declarator L_PAREN parameter_type_list R_PAREN) #f))
  (initializer
   ((L_BRACE initializer_list R_BRACE) #f)
   ((L_BRACE initializer_list COMMA R_BRACE) #f)
   ((assignment_expression) #f))
  (initializer_list
   ((designation initializer) #f)
   ((initializer) #f)
   ((initializer_list COMMA designation initializer) #f)
   ((initializer_list COMMA initializer) #f))
  (designation ((designator_list =) #f))
  (designator_list ((designator) #f) ((designator_list designator) #f))
  (designator ((L_BRACKET constant_expression R_BRACKET) #f) ((PERIOD IDENTIFIER) #f))
  (static_assert_declaration
   ((STATIC_ASSERT L_PAREN constant_expression COMMA STRING_LITERAL R_PAREN SEMI_COLON) #f))
  (statement
   ((labeled_statement) #f)
   ((compound_statement) #f)
   ((expression_statement) #f)
   ((selection_statement) #f)
   ((iteration_statement) #f)
   ((jump_statement) #f))
  (labeled_statement
   ((IDENTIFIER : statement) #f)
   ((CASE constant_expression : statement) #f)
   ((DEFAULT : statement) #f))
  (compound_statement ((L_BRACE R_BRACE) #f) ((L_BRACE block_item_list R_BRACE) #f))
  (block_item_list ((block_item) #f) ((block_item_list block_item) #f))
  (block_item ((declaration) #f) ((statement) #f))
  (expression_statement ((SEMI_COLON) #f) ((expression SEMI_COLON) #f))
  (selection_statement
   ((IF L_PAREN expression R_PAREN statement ELSE statement) #f)
   ((IF L_PAREN expression R_PAREN statement) #f)
   ((SWITCH L_PAREN expression R_PAREN statement) #f))
  (iteration_statement
   ((WHILE L_PAREN expression R_PAREN statement) #f)
   ((DO statement WHILE L_PAREN expression R_PAREN SEMI_COLON) #f)
   ((FOR L_PAREN expression_statement expression_statement R_PAREN statement) #f)
   ((FOR
     L_PAREN
     expression_statement
     expression_statement
     expression
     R_PAREN
     statement)
    #f)
   ((FOR L_PAREN declaration expression_statement R_PAREN statement) #f)
   ((FOR L_PAREN declaration expression_statement expression R_PAREN statement) #f))
  (jump_statement
   ((GOTO IDENTIFIER SEMI_COLON) #f)
   ((CONTINUE SEMI_COLON) #f)
   ((BREAK SEMI_COLON) #f)
   ((RETURN SEMI_COLON) #f)
   ((RETURN expression SEMI_COLON) #f))
  (translation_unit
   ((external_declaration) #f)
   ((translation_unit external_declaration) #f))
  (external_declaration ((function_definition) #f) ((declaration) #f))
  (function_definition
   ((declaration_specifiers declarator declaration_list compound_statement)
    #f)
   ((declaration_specifiers declarator compound_statement) #f))
  (declaration_list
   ((declaration) #f)
   ((declaration_list declaration) #f))))


#;
(parser
 (start translation-unit)
 (end EOF)
 (tokens Keywords Basic Special Punctuators)
 (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
          (raise-syntax-error "Error")))
 (grammar
  ;; trans unit & declaration
  (translation-unit [(external-declaration) #f]
                    [(translation-unit external-declaration) #f])
  (external-declaration [(function-definition) #f]
                        [(declaration) #f])
  (function-definition [(declaration-specifiers declarator declaration-list compound-statement) #f]
                       [(declaration-specifiers declarator compound-statement) #f])
  (declaration [(declaration-specifiers init-declarator-list) #f]
               [(declaration-specifiers) #f])
  (declaration-specifiers [(storage-class-specifier declaration-specifiers) #f]
                          [(storage-class-specifier) #f]
                          [(type-specifier declaration-specifiers) #f]
                          [(type-specifier) #f]
                          [(type-qualifier declaration-specifiers) #f]
                          [(type-qualifier) #f]
                          [(function-specifier declaration-specifiers) #f]
                          [(function-specifier) #f])
  (declarator [(pointer direct-declarator) #f]
              [(direct-declarator) #f])
  (declaration-list [(declaration) #f]
                    [(declaration-list declaration) #f])
  (init-declarator-list [(init-declarator) #f]
                        [(init-declarator-list COMMA init-declarator) #f])
  (direct-declarator [(identifier) #f]
                     [(L_BRACE declarator R_BRACE) #f]
                     [(direct-declarator L_BRACKET type-qualifier-list)])
  ;; specifier
  (storage-class-specifier [(typedef) #f]
                           [(extern) #f]
                           [(static) #f]
                           [(auto) #f]
                           [(register) #f])
  (type-specifier [(void) #f]
                  [(char) #f]
                  [(short) #f]
                  [(int) #f]
                  [(long) #f]
                  [(float) #f]
                  [(double) #f]
                  [(signed) #f]
                  [(unsigned) #f]
                  [(_Bool) #f]
                  [(_Complex) #f]
                  [(struct-or-union-specifier) #f]
                  [(enum-specifier) #f]
                  [(typedef-name) #f])
  (type-qualifier [(const) #f]
                  [(restrict) #f]
                  [(volatile) #f])
  (function-specifier [(inline) #f])
  ;; special
  (pointer [(* type-qualifier-list) #f]
           [(*) #f]
           [(* type-qualifier-list pointer) #f]
           [(* pointer) #f])
  ;; statements
  (compound-statement [(L_BRACE block-item-list R_BRACE)]
                      [(L_BRACE R_BRACE)])
  ))
