import ply.lex as lex

# Palavras reservadas do Pascal standard
reserved = {
    'program': 'PROGRAM',
    'var': 'VAR',
    'array': 'ARRAY',
    'of': 'OF',
    'begin': 'BEGIN',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'for': 'FOR',
    'to': 'TO',
    'downto': 'DOWNTO',
    'function': 'FUNCTION',

    # Operadores lógicos
    'not': 'NOT',
    'and': 'AND',
    'or': 'OR',
    'div': 'DIV',
    'mod': 'MOD',

    # I/O
    'readln': 'READLN',
    'writeln': 'WRITELN',

    # Booleanos
    'true': 'TRUE',
    'false': 'FALSE',

    # Tipos primitivos
    'integer': 'INTEGER_TYPE',
    'real': 'REAL_TYPE',
    'boolean': 'BOOLEAN_TYPE',
    'char': 'CHAR_TYPE',
    'string': 'STRING_TYPE',

    'length':'LENGTH'
}


# Lista de tokens, incluindo as palavras reservadas
tokens = [
    'IDENTIFICADOR',
    'INTEGER', 'REAL', 'STRING',

    # Operadores
    'MAIS', 'MENOS', 'VEZES', 'DIVIDIR',
    'MENOR', 'MAIOR', 'MENOR_IGUAL', 'MAIOR_IGUAL', 'IGUAL', 'DIFERENTE',
    'ATRIBUICAO',

    # Símbolos
    'PA', 'PF',
    'PRA', 'PRF',
    'PONTO_VIRGULA', 'DOIS_PONTO', 'VIRGULA', 'PONTO', 'PONTO_PONTO'
] + list(set(reserved.values()))


# Regras simples
t_MAIS = r'\+'
t_MENOS = r'-'
t_VEZES = r'\*'
t_DIVIDIR = r'/'
t_IGUAL = r'='
t_DIFERENTE = r'<>'
t_MENOR = r'<'
t_MAIOR = r'>'
t_MENOR_IGUAL = r'<='
t_MAIOR_IGUAL = r'>='
t_ATRIBUICAO  = r':='
t_PA = r'\('
t_PF = r'\)'
t_PRA = r'\['
t_PRF = r'\]'
t_PONTO_VIRGULA   = r';'
t_DOIS_PONTO  = r':'
t_VIRGULA  = r','
t_PONTO    = r'\.'
t_PONTO_PONTO = r'\.\.'

# Strings Pascal ('texto')
def t_STRING(t):
    # r"\'([^']|\'\')*\'"
    r"\'([^\']*)\'"
    t.value = t.value[1:-1]
    return t

# Números (inteiros e reais)
def t_REAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Identificadores ou palavras reservadas
def t_IDENTIFICADOR(t):
    r'[A-Za-z_][A-Za-z0-9_]*'
    t.type = reserved.get(t.value.lower(), 'IDENTIFICADOR')
    return t

# def t_ARGUMENT(t):
#     r"\'([^\']*)\'"
#     t.value = t.value[1:-1]
#     return t

# Comentários { } ou (* *)
def t_COMENTARIO(t):
    r'(\{[^}]*\})|(\(\*([^*]|\*+[^)])*\*\))'
    pass

# Ignorar whitespaces
t_ignore = ' \t\r'

# Nova linha
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Erro de caractere inválido
def t_error(t):
    print(f"Caractere invalido: {t.value[0]}")
    t.lexer.skip(1)

lexer = lex.lex()

