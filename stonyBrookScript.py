'''
Fanng Dai

Stony Brook University
CSE 307
Homework #5
Due Friday, June 29
'''

symbol_table = {}

class SemanticError(Exception):
    """
    This is the class of the exception that is raised when a semantic error
    occurs.
    """

################################################################################
# DATA TYPES
################################################################################
class Node(object):
    def __init__(self):
        pass

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class ExpressionNode(Node):
    def evaluate(self):
        return None

class NumNode(ExpressionNode):
    def __init__(self, v):
        if('.' in v):
            self.v = float(v)
        else:
            self.v = int(v)

    def evaluate(self):
        return self.v

class ListNode(ExpressionNode):
    def __init__(self, v):
        self.v = [v]

    def evaluate(self):
        l = []
        for v in self.v:
            if type(v) == list:
                l.append(v)
            else:
                l.append(v.evaluate())
        return l

class strNode(ExpressionNode):
    def __init__(self, v):
        # Remove the quotations
        self.v = str(v)[1:-1]

    def evaluate(self):
        return self.v

class BoolNode(ExpressionNode):
    def __init__(self, v):
        if v=='True':
            self.v = True
        else:
            self.v = False

    def evaluate(self):
        return self.v

class IDNode(ExpressionNode):
    def __init__(self, vid):
        self.vid = vid

    def evaluate(self):
        return symbol_table[self.vid]

################################################################################
# OPERATORS
################################################################################
class orOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, bool) and isinstance(self.right, bool):
            return self.left or self.right
        else:
            raise SemanticError()

class andOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, bool) and isinstance(self.right, bool):
            return self.left and self.right
        else:
            raise SemanticError()

class notOp(ExpressionNode):
    def __init__(self, v):
        self.v = v.evaluate()

    def evaluate(self):
        if isinstance(self.v, bool):
            return not self.v
        else:
            raise SemanticError()

class comparisonOp(ExpressionNode):
    def __init__(self, op, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()
        self.op = op

    def evaluate(self):
        if not (isinstance(self.left, int) or isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, float) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, str) and isinstance(self.right, str)):
            raise SemanticError()

        if (self.op == '<'):
            return self.left < self.right
        elif (self.op == '<='):
            return self.left <= self.right
        elif (self.op == '=='):
            return self.left == self.right
        elif (self.op == '<>'):
            return self.left != self.right
        elif (self.op == '>'):
            return self.left > self.right
        elif (self.op == '>='):
            return self.left >= self.right

class inOp(ExpressionNode):
    def __init__(self, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()

    def evaluate(self):
        v1 = self.v1
        v2 = self.v2
        return v1 in v2

class addOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, float) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, str) and isinstance(self.right, str) or \
            isinstance(self.left, list) and isinstance(self.right, list):
            return self.left + self.right
        else:
            raise SemanticError()

class subOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, float):
            return self.left - self.right
        else:
            raise SemanticError()

class flDivOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, float) or \
            self.right != 0:
            return self.left // self.right
        else:
            raise SemanticError()

class modOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, float) or \
            self.right != 0:
            return self.left % self.right
        else:
            raise SemanticError()

class powOp(ExpressionNode):
    def __init__(self, base, pow):
        self.base = base.evaluate()
        self.pow = pow.evaluate()

    def evaluate(self):
        return self.base ** self.pow

class mulOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, float):
            return self.left * self.right
        else:
            raise SemanticError()

class divOp(ExpressionNode):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, int) or \
            isinstance(self.left, int) and isinstance(self.right, float) or \
            isinstance(self.left, float) and isinstance(self.right, float) or \
            self.right != 0:
            return self.left / self.right
        else:
            raise SemanticError()

class indexOp(ExpressionNode):
    def __init__(self, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()

    def evaluate(self):
        if isinstance(self.v1, str) or \
            isinstance(self.v1, list) or \
            isinstance(self.v2, int):
            try:
                return self.v1[self.v2]
            except IndexError:
                raise SemanticError()
        else:
            raise SemanticError()

################################################################################
# CONTROL FLOW
################################################################################
class BlockNode(Node):
    def __init__(self, s):
        self.s1 = [s]

    def execute(self):
        for statement in self.s1:
            statement.execute()

class StatementNode(Node):
    def execute(self):
        pass

class PrintNode(Node):
    def __init__(self, v):
        self.v = v

    def execute(self):
        print(str(self.v.evaluate()))

class AssignNode(StatementNode):
    def __init__(self, vnode, exp):
        self.vnode = vnode
        self.exp = exp

    def execute(self):
        symbol_table[self.vnode.vid] = self.exp.evaluate()

class AssignToListNode(StatementNode):
    def __init__(self, vnode, ind, exp):
        self.vnode = vnode
        self.ind = ind
        self.exp = exp

    def execute(self):
        symbol_table[self.vnode.vid][self.ind.evaluate()] = self.exp.evaluate()

class IfNode(StatementNode):
    def __init__(self, cond, block):
        self.cond = cond
        self.block = block

    def execute(self):
        if(self.cond.evaluate()):
            self.block.execute()

class IfElseNode(StatementNode):
    def __init__(self, cond, iblock, eblock):
        self.cond = cond
        self.iblock = iblock
        self.eblock = eblock

    def execute(self):
        if(self.cond.evaluate()):
            self.iblock.execute()
        else:
            self.eblock.execute()

class WhileNode(StatementNode):
    def __init__(self, cond, block):
        self.cond = cond
        self. block = block

    def execute(self):
        while self.cond.evaluate():
            self.block.execute()
################################################################################
# TOKENS
################################################################################
reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    # 'not' : 'NOT',
    # 'and' : 'AND',
    # 'or' : 'OR',
    # 'in' : 'IN',
    'print' : 'PRINT'
}

tokens = [
    'NUM', 'STR',
    'COMMA',
    'LPAREN', 'RPAREN',
    'LBLOCK','RBLOCK',
    'BOOL',
    'POW',
    'MUL','DIV',
    'MOD',
    'FLDIV',
    'ADD','SUB',
    'IN',
    'LT', 'LE', 'EQ', 'NE', 'GT', 'GE',
    'NOT',
    'AND',
    'OR',
    'ASSIGN',
    'LBRACE', 'RBRACE',
    'SEMI',
    'ID'
    ] + list(reserved.values())

t_COMMA = ','
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBLOCK = r'\['
t_RBLOCK = r'\]'
t_POW = r'\*\*'
t_MUL = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_FLDIV = r'//'
t_ADD = r'\+'
t_SUB = r'-'
t_IN = r'in'
t_LT = r'<'
t_LE = r'<='
t_EQ = r'=='
t_NE = r'<>'
t_GT = r'>'
t_GE = r'>='
t_NOT = r'not'
t_AND = r'and'
t_OR = r'or'
t_ASSIGN = r'='
t_LBRACE = r'{'
t_RBRACE = r'}'
t_SEMI = r';'

# Ignored characters
t_ignore = " \t"

def t_NUM(t):
    r'-?\d*(\d\.|\.\d)\d* | -?\d+'
    # r'\d*(\d\.|\.\d)\d* | \d+'
    try:
        t.value = NumNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_STR(t):
    r'((\"[^\"]*\")|(\'[^\']*\'))'
    t.value = strNode(t.value)
    return t

def t_BOOL(t):
    r'(True|False)'
    t.value = BoolNode(t.value)
    return t

def t_ID(token):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    token.type = reserved.get(token.value, 'ID')
    token.value = IDNode(token.value)
    return token

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    raise SyntaxError("SYNTAX ERROR")

################################################################################
# PARSING RULES
################################################################################
# Build the lexer
import ply.lex as lex
lex.lex()

precedence = (
    # Lowest Associative
    ('right', 'ASSIGN'),
    ('left', 'PRINT'),
    ('left', 'OR' ),                                    # a or b    Boolean OR
    ('left', 'AND'),                                    # a and b   Boolean AND
    ('left', 'NOT'),                                    # not a     Boolean NOT
    ('left', 'LT', 'LE', 'EQ', 'NE', 'GT', 'GE'),       # a<b, a<=b, a==b, a<>b, a>b, a>=b  Comparison (works for numbers and strings like in python)
    ('left', 'IN'),                                     # a in b    Evaluates to true if it find a variable in the specified sequence and false o.w.
    ('left','ADD','SUB'),                               # a+b, a-b  Addition & Subtraction
    ('left', 'FLDIV'),                                  # a//b      Floor Division, - The division of operands where the result is quotient in which the digits after the decimal point are removed
    ('left', 'MOD'),                                    # a%b       Modulus - Divides left hand operand by right hand operand and returns remainder
    ('left','MUL','DIV'),                               # a*b, a/b  Multiplication & Division
    ('right', 'POW'),                                   # a**b      Exponent Performs exponential (power) calculation on operators = a to the power b. 2**3**4= 2**(3**4) = 2417851639229258349412352
    ('left', 'LBLOCK', 'RBLOCK'),                       # a[b]      Indexing. B may be any expression
    ('left', 'LPAREN', 'RPAREN'),                       # (expression)  A parenthesized expression
    ('left', 'LBRACE', 'RBRACE')
    # Highest Associative
    )

def p_block(t):
    '''block : LBRACE inblock RBRACE'''
    t[0] = t[2]

def p_emptyblock(t):
    '''block : LBRACE RBRACE'''
    # Literally do nothing
    t[0] = BlockNode([])
    t[0].s1 = []

def p_inblock(t):
    '''inblock : statement inblock'''
    t[0] = t[2]
    t[0].s1.insert(0, t[1])

def p_inblock2(t):
    '''inblock : statement'''
    t[0] = BlockNode(t[1])

def p_smt(t):
    '''statement : print_smt
                 | assign_smt
                 | assign_to_list
                 | if_smt
                 | ifelse
                 | while_smt
                 | solo_block'''
    t[0] = t[1]

# def p_print_smt(t):
#     '''print_smt : PRINT LPAREN expression RPAREN'''
#     t[0] = PrintNode(t[3])

def p_statement_print(t):
    '''print_smt : PRINT LPAREN expression RPAREN SEMI %prec PRINT'''
    t[0] = PrintNode(t[3])

def p_statement_assign(t):
    '''assign_smt : ID ASSIGN expression SEMI %prec ASSIGN'''
    t[0] = AssignNode(t[1], t[3])

def p_statement_assign_to_list(t):
    '''assign_to_list : ID LBLOCK expression RBLOCK ASSIGN expression SEMI %prec ASSIGN'''
    t[0] = AssignToListNode(t[1], t[3], t[6])

def p_statement_while(t):
    '''while_smt : WHILE LPAREN expression RPAREN block'''
    t[0] = WhileNode(t[3], t[5])

def p_statement_if(t):
    '''if_smt : IF LPAREN expression RPAREN block'''
    t[0] = IfNode(t[3], t[5])

def p_statement_solo_block(t):
    '''solo_block : block'''
    t[0] = BlockNode(t[1])

def p_statement_ifelse(t):
    '''ifelse : if_smt ELSE block'''
    t[0] = IfElseNode(t[1].cond, t[1].block, t[3])

# def p_empty(p):
#     '''empty : '''
#     pass

################################################################################
# HW#4
################################################################################

# def p_statement_expr(t):
#     '''statement : expression'''
#     if type(t[1]) != list:
#         t[1] = t[1].evaluate()
#         if type(t[1]) == str:
#             t[1] = "'" + t[1] + "'"
#     print(t[1])

def p_expression_type(t):
    '''expression : NUM
                  | BOOL
                  | STR
                  | indexing
                  | list
                  | ID
                  '''
    t[0] = t[1]

def p_expression_group(t):
    '''expression : LPAREN expression RPAREN'''
    t[0] = t[2]

# List
def p_list(t):
    '''list : LBLOCK in_list RBLOCK'''
    t[0] = t[2]

def p_empty_list(t):
    '''list : LBLOCK RBLOCK'''
    t[0] = []

def p_in_list(t):
    '''in_list : expression'''
    t[0] = ListNode(t[1])

def p_in_list2(t):
    '''in_list : expression COMMA in_list'''
    t[3].v.insert(0,t[1])
    t[0] = t[3]

def p_expression_index(t):
    ''' indexing : STR LBLOCK expression RBLOCK
                 | list LBLOCK expression RBLOCK
                 | indexing LBLOCK expression RBLOCK '''
    t[0] = indexOp(t[1], t[3])

def p_expression_maybeSub(t):
    '''expression : NUM NUM'''
    if t[2].evaluate() < 0:
        t[0] = addOp(t[1], t[2])

def p_expression_binop(t):
    '''expression : expression ADD expression
                  | expression SUB expression
                  | expression MUL expression
                  | expression DIV expression
                  | expression FLDIV expression
                  | expression MOD expression
                  | expression POW expression'''
    if t[2] == '+':
        t[0] = addOp(t[1], t[3])
    elif t[2] == '-':
        t[0] = subOp(t[1], t[3])
    elif t[2] == '*':
        t[0] = mulOp(t[1], t[3])
    elif t[2] == '/':
        t[0] = divOp(t[1], t[3])
    elif t[2] == '//':
        t[0] = flDivOp(t[1], t[3])
    elif t[2] == '%':
        t[0] = modOp(t[1], t[3])
    elif t[2] == '**':
        t[0] = powOp(t[1], t[3])

def p_expression_comparison(t):
    '''expression : expression LT expression
                  | expression LE expression
                  | expression EQ expression
                  | expression NE expression
                  | expression GT expression
                  | expression GE expression'''
    t[0] = comparisonOp(t[2], t[1], t[3])

def p_expression_conjunction(t):
    '''expression : expression OR expression
                  | expression AND expression
                  | NOT expression
                  | expression IN expression'''
    if t[1] == 'not':
        t[0] = notOp(t[2])
    elif t[2] == 'or':
        t[0] = orOp(t[1], t[3])
    elif t[2] == 'and':
        t[0] = andOp(t[1], t[3])
    elif t[2] == 'in':
        t[0] = inOp(t[1], t[3])

def p_error(t):
    print("###")
    raise SyntaxError

################################################################################
# MAIN
################################################################################
import ply.yacc as yacc
parser = yacc.yacc()

import sys

# Open file if exist
if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
try:
    file = open(sys.argv[1], 'r')
except(IOError):
    exit()

# Parse & print
try:
    ast = yacc.parse(file.read())
    ast.execute()
except SemanticError:
    print("SEMANTIC ERROR A")
except SyntaxError:
    print("SYNTAX ERROR B")
except Exception:
    print("SEMANTIC ERROR C")

file.close()
