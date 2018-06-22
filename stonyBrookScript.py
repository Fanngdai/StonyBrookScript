'''
Fanng Dai

Stony Brook University
CSE 307
Homework #4
Due Friday, June 22
'''

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
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class NumNode(Node):
    def __init__(self, v):
        if('.' in v):
            self.v = float(v)
        else:
            self.v = int(v)

    def evaluate(self):
        return self.v

class ListNode(Node):
    def __init__(self, v):
        self.v = [v]

    def evaluate(self):
        l = []
        for v in self.v:
            l.append(v.evaluate())
        return l

class strNode(Node):
    def __init__(self, v):
        # Remove the quotations
        self.v = str(v)[1:-1]

    def evaluate(self):
        return self.v

class BoolNode(Node):
    def __init__(self, v):
        if v=='True':
            self.v = True
        else:
            self.v = False

    def evaluate(self):
        return self.v

# class PrintNode(Node):
#     def __init__(self, v):
#         self.v = v
#
#     def execute(self):
#         self.v = self.v.evaluate()
#         print(self.v)

################################################################################
# OPERATORS
################################################################################
class orOp(Node):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, bool) and isinstance(self.right, bool):
            return self.left or self.right
        else:
            raise SemanticError()

class andOp(Node):
    def __init__(self, left):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        if isinstance(self.left, bool) and isinstance(self.right, bool):
            return self.left and self.right
        else:
            raise SemanticError()

class notOp(Node):
    def __init__(self, v):
        self.v = v.evaluate()

    def evaluate(self):
        if isinstance(self.v, bool):
            return not self.v
        else:
            raise SemanticError()

class comparisonOp(Node):
    def __init__(self, op, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()
        self.op = op

    def evaluate(self):
        if not (isinstance(self.left, int) and isinstance(self.right, int) or \
            isinstance(self.left, float) and isinstance(self.right, float) or \
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

class inOp(Node):
    def __init__(self, v1, v2):
        self.v1 = v1.evaluate()
        self.v2 = v2.evaluate()

    def evaluate(self):
        v1 = self.v1
        v2 = self.v2
        return v1 in v2

class addOp(Node):
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

class subOp(Node):
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

class flDivOp(Node):
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

class modOp(Node):
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

class powOp(Node):
    def __init__(self, left, right):
        self.left = left.evaluate()
        self.right = right.evaluate()

    def evaluate(self):
        return self.left ** self.right

class mulOp(Node):
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

class divOp(Node):
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

class indexOp(Node):
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
# TOKENS
################################################################################
tokens = (
    'OR', 'AND', 'NOT', 'IN',
    'LT', 'LE', 'EQ', 'NE', 'GT', 'GE',
    'ADD','SUB','MUL','DIV',
    'FLDIV', 'MOD', 'POW',
    'LBRACKET','RBRACKET', 'COMMA',
    'LPAREN', 'RPAREN',
    'NUM', 'STR', 'BOOL'
    )

t_OR = r'or'
t_AND = r'and'
t_NOT = r'not'
t_IN = r'in'
t_LT = r'<'
t_LE = r'<='
t_EQ = r'=='
t_NE = r'<>'
t_GT = r'>'
t_GE = r'>='
t_ADD = r'\+'
t_SUB = r'-'
t_MUL = r'\*'
t_DIV = r'/'
t_FLDIV = r'\\'
t_MOD = r'%'
t_POW = r'\*\*'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = ','
t_LPAREN = r'\('
t_RPAREN = r'\)'
# Ignored characters
t_ignore = " \t"

def t_NUM(t):
    r'-?\d*(\d\.|\.\d)\d* | \d+'
    try:
        t.value = NumNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_BOOL(t):
    r'(True|False)'
    t.value = BoolNode(t.value)
    return t

def t_STR(t):
    r'("([^\\"]*|\\.)*")|(\'([^\\"]*|\\.)*\')'
    t.value = strNode(t.value)
    return t

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
    ('left', 'LBRACKET', 'RBRACKET'),                   # a[b]      Indexing. B may be any expression
    ('left', 'LPAREN', 'RPAREN')                        # (expression)  A parenthesized expression
    # Highest Associative
    )

def p_statement_expr(t):
    '''statement : expression'''
    t[1] = t[1].evaluate()
    if type(t[1]) == str:
        t[1] = "'" + t[1] + "'"
    print(t[1])

def p_expression_type(t):
    '''expression : NUM
                  | BOOL
                  | STR
                  | indexing
                  | list
                  '''
    t[0] = t[1]

def p_expression_group(t):
    '''expression : LPAREN expression RPAREN'''
    t[0] = t[2]

# List
def p_list(t):
    '''list : LBRACKET in_list RBRACKET'''
    t[0] = t[2]

def p_in_list(t):
    '''in_list : expression'''
    t[0] = ListNode(t[1])

def p_in_list2(t):
    '''in_list : expression COMMA in_list'''
    t[3].v.insert(0,t[1])
    t[0] = t[3]
    # t[0] = t[1] + t[3].v

def p_expression_index(t):
    ''' indexing : STR LBRACKET expression RBRACKET
                 | list LBRACKET expression RBRACKET '''
    t[0] = indexOp(t[1], t[3])

def p_expression_binop(t):
    '''expression : expression ADD expression
                  | expression SUB expression
                  | expression MUL expression
                  | expression DIV expression
                  | expression FLDIV expression
                  | expression MOD expression
                  | expression POW expression'''
    # t[0] = ('+', t[1], t[3])
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
    '''expression : BOOL OR BOOL
                  | BOOL AND BOOL
                  | NOT BOOL
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
    raise SyntaxError("SYNTAX ERROR")

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
for line in file:
    try:
        l = line.strip()
        lex.input(l)
        while True:
            token = lex.token()
            if not token:
                break
        ast = yacc.parse(l)
        # ast.execute()
        # parser.parse(l)
    except SemanticError:
        print("SEMANTIC ERROR")
    except Exception:
        print("SYNTAX ERROR")

file.close()
