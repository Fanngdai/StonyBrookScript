'''
Fanng Dai
Stony Brook University
CSE 307
Homework #5
Due Friday, June 29
'''

class SemanticError(Exception):
    """
    This is the class of the exception that is raised when a semantic error
    occurs.
    """

################################################################################
# DATA TYPES
################################################################################
class Node:
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
    def __init__(self, val):
        if('.' in val):
            self.val = float(val)
        else:
            self.val = int(val)

    def evaluate(self):
        return self.val

class ListNode(ExpressionNode):
    def __init__(self, val):
        self.val = val

    def evaluate(self):
        res = []
        for n in self.val:
            if type(n) == list:
                res.append(n)
            else:
                res.append(n.evaluate())
        return res

class StringNode(ExpressionNode):
    def __init__(self,val):
        # Remove the quotations
        self.val = str(val)[1:-1]

    def evaluate(self):
        return self.val

class BoolNode(ExpressionNode):
    def __init__(self, val):
        if val == 'True':
            self.val = True
        else:
            self.val = False

    def evaluate(self):
        return self.val

class BlockNode(Node):
    def __init__(self, s):
        self.s1 = [s]

    def execute(self):
        for statement in self.s1:
            statement.execute()

################################################################################
# OPERATORS
################################################################################
variables = {}

class IDNode(ExpressionNode):
    def __init__(self, vid):
        self.vid = vid

    def evaluate(self):
        return variables[self.vid]

class BoolOp(ExpressionNode):
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if isinstance(left, bool) and isinstance(right, bool):
            if self.op.vid == 'and':
                return  left and right
            elif self.op.vid == 'or':
                return left or right
        else:
            raise SemanticError()

class NotOp(ExpressionNode):
    def __init__(self, op, val):
        self.op = op
        self.val = val

    def evaluate(self):
        res = self.val.evaluate()
        if isinstance(res, bool):
            return not res
        else:
            raise SemanticError()

class ComparisonOp(ExpressionNode):
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()

        if isinstance(left, int) or isinstance(right, int) or \
            isinstance(left, float) and isinstance(right, float) or \
            isinstance(left, int) and isinstance(right, float) or \
            isinstance(left, float) and isinstance(right, int) or \
            isinstance(left, str) and isinstance(right, str):
            if self.op == '<':
                return left < right
            elif self.op == '<=':
                return left <= right
            elif self.op == '==':
                return left == right
            elif self.op == '<>':
                return left != right
            elif self.op == '>':
                return left > right
            elif self.op == '>=':
                return left >= right
        else:
            raise SemanticError()

class ContainsNode(ExpressionNode):
    def __init__(self, obj, tgt):
        self.obj = obj
        self.tgt = tgt

    def evaluate(self):
        return self.tgt.evaluate() in self.obj.evaluate()

class AddOp(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()

        if isinstance(left, int) and isinstance(right, int) or \
            isinstance(left, float) and isinstance(right, float) or \
            isinstance(left, int) and isinstance(right, float) or \
            isinstance(left, float) and isinstance(right, int) or \
            isinstance(left, str) and isinstance(right, str) or \
            isinstance(left, list) and isinstance(right, list):
            return left + right
        else:
            raise SemanticError()

class SubOp(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()

        if isinstance(left, int) and isinstance(right, int) or \
            isinstance(left, float) and isinstance(right, int) or \
            isinstance(left, int) and isinstance(right, float) or \
            isinstance(left, float) and isinstance(right, float):
            return left - right
        else:
            raise SemanticError()

class PowOp(ExpressionNode):
    def __init__(self, base, exp):
        self.base = base
        self.exp = exp

    def evaluate(self):
        return self.base.evaluate() ** self.exp.evaluate()

class MulOp(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()

        if isinstance(left, int) and isinstance(right, int) or \
            isinstance(left, float) and isinstance(right, int) or \
            isinstance(left, int) and isinstance(right, float) or \
            isinstance(left, float) and isinstance(right, float):
            return left * right
        else:
            raise SemanticError()

class Div_FlDiv_ModOp(Node):
    def __init__(self, op, left, right):
        self.left = left
        self.right = right
        self.op = op

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()

        if isinstance(left, int) and isinstance(right, int) or \
            isinstance(left, float) and isinstance(right, int) or \
            isinstance(left, int) and isinstance(right, float) or \
            isinstance(left, float) and isinstance(right, float) or \
            right != 0:
            if self.op == '//':
                return left // right
            elif self.op == '%':
                return left % right
            elif self.op == '/':
                return left / right
        else:
            raise SemanticError()

class IndexNode(ExpressionNode):
    def __init__(self, obj, ind):
        self.obj = obj
        self.ind = ind

    def evaluate(self):
        obj = self.obj.evaluate()
        ind = self.ind.evaluate()
        if isinstance(obj, str) or \
            isinstance(obj, list) or \
            isinstance(ind, int):
            try:
                return obj[ind]
            except IndexError:
                raise SemanticError()
        else:
            raise SemanticError()

################################################################################
# NOT NEEDED
################################################################################
class UnaryOpNode(ExpressionNode):
    def __init__(self, op, val):
        self.op = op
        self.val = val

    def evaluate(self):
        return -self.val.evaluate()

################################################################################
# CONTROL FLOW
################################################################################
class StatementNode(Node):
    def execute(self):
        pass

class AssignNode(StatementNode):
    def __init__(self, vnode, exp):
        self.vnode = vnode
        self.exp = exp

    def execute(self):
        variables[self.vnode.vid] = self.exp.evaluate()

class AssignToListNode(StatementNode):
    def __init__(self, vnode, ind, exp):
        self.vnode = vnode
        self.exp = exp
        self.ind = ind

    def execute(self):
        variables[self.vnode.vid][self.ind.evaluate()] = self.exp.evaluate()

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

class PrintNode(StatementNode):
    def __init__(self, exp):
        self.e = exp

    def execute(self):
        print(self.e.evaluate())

################################################################################
# TOKENS
################################################################################
reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'not' : 'NOT',
    'and' : 'AND',
    'or' : 'OR',
    'in' : 'IN',
    'print' : 'PRINT'
}

tokens = [
    'NUM', 'STRING',
    'COMMA',
    'LPAREN', 'RPAREN',
    'LBLOCK', 'RBLOCK',
    'BOOL',
    'EXP',
    'MULT', 'DIV',
    'MOD',
    'INTDIV',
    'ADD', 'SUB',
    'LESS', 'LESSEQ', 'EQUAL', 'NOTEQUAL', 'GREATER', 'GREATEREQ',
    'ASSIGN',
    'LBRACE', 'RBRACE',
    'SEMI',
    'ID'
] + list(reserved.values())

t_COMMA = r','
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBLOCK = r'\['
t_RBLOCK = r'\]'
t_EXP = r'\*\*'
t_MULT = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_INTDIV = r'//'
t_ADD = r'\+'
t_SUB = r'-'
t_LESS = r'<'
t_LESSEQ = r'<='
t_EQUAL = r'=='
t_NOTEQUAL = r'<>'
t_GREATER = r'>'
t_GREATEREQ = r'>='
t_ASSIGN = r'='
t_LBRACE = r'{'
t_RBRACE = r'}'
t_SEMI = r';'
t_ignore = ' \t'

def t_NUM(t):
    r'(\d*(\d\.|\.\d)\d*)|(\d+)'
    try:
        t.value = NumNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_STRING(token):
    r'(\"[^\"]*\") | (\'[^\']*\')'
    token.value = StringNode(token.value)
    return token

def t_BOOL(token):
    r'True|False'
    token.value = BoolNode(token.value)
    return token

def t_ID(token):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    token.type = reserved.get(token.value, 'ID')
    token.value = IDNode(token.value)
    return token

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(token):
    print("Illegal character '%s'" % token.value[0])
    token.lexer.skip(1)

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
    ('left', 'UNOT'),
    ('left', 'LESS', 'LESSEQ', 'EQUAL', 'NOTEQUAL', 'GREATER', 'GREATEREQ'),    # a<b, a<=b, a==b, a<>b, a>b, a>=b  Comparison (works for numbers and strings like in python)
    ('left', 'IN'),                                     # a in b    Evaluates to true if it find a variable in the specified sequence and false o.w.
    ('left', 'ADD', 'SUB'),                             # a+b, a-b  Addition & Subtraction
    ('left', 'INTDIV'),                                 # a//b      Floor Division, - The division of operands where the result is quotient in which the digits after the decimal point are removed
    # ('left', 'UMINUS'),
    ('left', 'MOD'),                                    # a%b       Modulus - Divides left hand operand by right hand operand and returns remainder
    ('left', 'MULT', 'DIV'),                            # a*b, a/b  Multiplication & Division
    ('right', 'EXP'),                                   # a**b      Exponent Performs exponential (power) calculation on operators = a to the power b. 2**3**4= 2**(3**4) = 2417851639229258349412352
    ('left', 'LBLOCK', 'RBLOCK'),                       # a[b]      Indexing. B may be any expression
    ('left', 'LIST'),
    ('left', 'INDEX')
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

def p_empty(p):
    '''empty : '''
    pass

def p_expression_inlist(t):
    '''expression : expression IN expression %prec IN'''
    t[0] = ContainsNode(t[3], t[1])

def p_expression_index(t):
    '''expression : STRING LBLOCK expression RBLOCK
                  | expression LBLOCK expression RBLOCK %prec INDEX'''
    t[0] = IndexNode(t[1], t[3])

def p_expression_group(t):
    '''expression : LPAREN expression RPAREN'''
    t[0] = t[2]

def p_expression_list(t):
    '''expression : LBLOCK expression term RBLOCK'''
    t[3].insert(0,t[2])
    t[0] = t[3]
    t[0] = ListNode(t[0])

def p_expression_list2(t):
    '''expression : LBLOCK RBLOCK %prec LIST'''
    t[0] = ListNode([])

def p_expression_term(t):
    '''term : COMMA expression term'''
    t[0] = t[2]
    t[3].insert(0,t[0])
    t[0] = t[3]

def p_expression_term2(t):
    '''term : empty %prec LIST'''
    t[0] = []

def p_emptyblock(t):
    '''
    block : LBRACE RBRACE
    '''
    # Literally do nothing
    t[0] = BlockNode([])
    t[0].s1 = []

def p_expression_compare(t):
    ''' expression : expression LESS expression
                   | expression LESSEQ expression
                   | expression EQUAL expression
                   | expression NOTEQUAL expression
                   | expression GREATER expression
                   | expression GREATEREQ expression'''
    t[0] = ComparisonOp(t[2], t[1], t[3])

def p_expression_boolop(t):
    '''expression : expression AND expression
                  | expression OR expression'''
    t[0] = BoolOp(t[2], t[1], t[3])

def p_expression_unot(t):
    '''expression : NOT expression %prec UNOT'''
    t[0] = NotOp(t[1], t[2])

def p_expression_AddSub(t):
    '''expression : expression ADD expression
                  | expression SUB expression'''
    if t[2] == '+':
        t[0] = AddOp(t[1], t[3])
    elif t[2] == '-':
        t[0] = SubOp(t[1], t[3])

def p_expression_mult(t):
    '''expression : expression MULT expression'''
    t[0] = MulOp(t[1], t[3])

def p_expression_div(t):
    '''expression : expression DIV expression
                  | expression MOD expression
                  | expression INTDIV expression'''
    t[0] = Div_FlDiv_ModOp(t[2], t[1], t[3])

def p_expression_exp(t):
    '''expression : expression EXP expression'''
    t[0] = PowOp(t[1], t[3])

# def p_expression_unaryminus(t):
#     '''expression : SUB expression %prec UMINUS'''
#     t[0] = UnaryOpNode(t[1], t[2])

def p_expression_type(t):
    '''expression : STRING
                  | NUM
                  | BOOL
                  | ID'''
    t[0] = t[1]

def p_error(p):
    print(p)
    raise SyntaxError

################################################################################
# MAIN
################################################################################
import ply.yacc as yacc
parser = yacc.yacc()

import sys

if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
try:
    file = open(sys.argv[1], 'r')
except(IOError):
    exit()

try:
    ast = yacc.parse(file.read())
    ast.execute()
except SyntaxError:
    print("SYNTAX ERROR")
except Exception:
    print("SEMANTIC ERROR")

file.close()
