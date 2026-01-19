import ply.yacc as yacc
from analexPASCAL import tokens, lexer

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('right', 'NOT'),
    ('nonassoc', 'IGUAL', 'DIFERENTE', 'MENOR', 'MENOR_IGUAL', 'MAIOR', 'MAIOR_IGUAL'),
    ('left', 'MAIS', 'MENOS'),
    ('left', 'VEZES', 'DIVIDIR', 'DIV', 'MOD'),
)

start = 'start_'

# -------------------------------------------------
# Programa principal
# -------------------------------------------------
def p_start(p):
    """
    start_ : PROGRAM IDENTIFICADOR PONTO_VIRGULA declaracoes BEGIN bloco_comandos END PONTO
    """
    # trata declaracoes para que declaracao de funcao nao interfira na main 
    dec_fun = []
    dec_var = []
    i = 0
    while i < len(p[4]):
        if p[4][i] == 'f':
            i += 1
            while i < len(p[4]) and p[4][i] != 'f':
                dec_fun.append(p[4][i])
                i += 1
        else:
            dec_var.append(p[4][i])
        i += 1
    final_code = ['START'] + dec_var + p[6] + ['STOP'] + dec_fun
    print("\n".join(final_code))


# -------------------------------------------------
# Bloco de comandos 
# -------------------------------------------------
def p_bloco_comandos_vazio(p):
    """
    bloco_comandos :
    """
    p[0] = []

def p_bloco_comandos(p):
    """
    bloco_comandos : lista_comandos_pv comando
                   | lista_comandos_pv
                   | comando
    """
    if len(p) == 3:
        p[0] = p[1]+p[2]
    else:
        p[0] = p[1]


def p_lista_comandos_one(p):
    """ 
    lista_comandos_pv : comando_pv
    """
    p[0] = p[1]

def p_lista_comandos(p):
    """
    lista_comandos_pv : lista_comandos_pv comando_pv
    """
    p[0] = p[1] + p[2]
    

# -------------------------------------------------
# Comandos
# -------------------------------------------------
def p_comando_pv(p):
    """
    comando_pv : comando PONTO_VIRGULA
    """
    p[0] = p[1]

def p_comando(p):
    """
    comando : atribuicao
            | leitura
            | escrita
            | if_h
            | while_h
            | for_h
            | return
            | BEGIN bloco_comandos END
    """
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]


# verificar identificador existe
def p_atribuicao_funcao(p):
    """
    atribuicao : IDENTIFICADOR ATRIBUICAO chamada_funcao
    """
    variavel = p[1]

    tipo_fun = parser.labels_f[p[3]]

    #verificar se a variavel foi declarada anteriormente
    if variavel not in parser.regists: 
        print(f"Erro p_atribuicao_funcao: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return
    #resgata tipo da variavel 
    tipo_var = parser.var_types[variavel]

    if tipo_var != tipo_fun:
        print(f"Erro: atribuição incompatível. Variável '{variavel}' é '{tipo_var}' mas funcao retorna um '{tipo_fun}'.")
        parser.success = False

    p[0] = [f"PUSHA {p[3]}","CALL"]+[f"STOREG {parser.regists[variavel]}"]

def p_atribuicao(p):
    """
    atribuicao : IDENTIFICADOR ATRIBUICAO expressao
    """
    variavel = p[1]
    expr, tipo_expr = p[3]
    
    #verificar se a variavel foi declarada anteriormente
    if variavel not in parser.regists: 
        print(f"Erro p_atribuicao: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return
    
    #resgata tipo da variavel 
    tipo = parser.var_types[variavel]

    if tipo != tipo_expr:
        print(f"Erro: atribuição incompatível. Variável '{variavel}' é '{tipo}' mas expressão é '{tipo_expr}'.")
        parser.success = False

    p[0] = expr + [f"STOREG {parser.regists[variavel]}"]

def p_leitura(p):
    """
    leitura : READLN PA IDENTIFICADOR PF
            | READLN PA IDENTIFICADOR PRA expressao PRF PF
    """
    if len(p) == 5:
        argumento = p[3]
        #verifica se foi declarado anteriormente
        if argumento not in parser.regists:
            print(f"Erro p_leitura: variável '{argumento}' não declarada.")
            parser.sucess = False
            p[0] = []
            return

        indice = parser.regists[argumento]
        tipo = parser.var_types[argumento]

        cod_traducao = ["READ"]
        if tipo == "integer" or tipo == "boolean":
            cod_traducao.append("ATOI") #tranforma string em inteiro
        elif tipo == "real":
            cod_traducao.append("ATOF") #tranforma string em float
        elif tipo == "string":
            pass    
        else:
            print(f"Erro: tipo '{tipo}' não suportado em READLN.")
            parser.success = False

        cod_traducao.append(f"STOREG {indice}")
        p[0] = cod_traducao
    else:
        argumento = p[3]
        expr, tipo_expr = p[5]

        if argumento not in parser.regists or parser.var_types[argumento][0] != 'array':
            print(f"Erro p_leitura: variável '{argumento}' não é um array ou não foi declarada.")
            parser.success = False
            return

        if tipo_expr != 'integer':
            print(f"Erro: índice de array '{argumento}' deve ser do tipo integer.")
            parser.success = False

        start = parser.var_types[argumento][1]
        base_address = parser.regists[argumento]
        element_type = parser.var_types[argumento][3]

        cod_traducao = ["PUSHGP"] + expr + [f"PUSHI {start - base_address}", "SUB", "READ"]

        if element_type == "integer" or element_type == "boolean":
            cod_traducao.append("ATOI")
        elif element_type == "real":
            cod_traducao.append("ATOF")
        elif element_type == "string":
            pass
        else:
            print(f"Erro: tipo de elemento desconhecido '{element_type}' no array '{argumento}'.")
            parser.success = False
            return

        cod_traducao.append("STOREN")
        p[0] = cod_traducao


def p_escrita(p):
    """
    escrita : WRITELN PA argumentos_escrita PF
    """
    cod = []
    for arg in p[3]:
        if isinstance(arg, str) and arg.startswith('"'):
            cod.append(f'PUSHS {arg}')
            cod.append('WRITES')

        elif isinstance(arg, tuple) and arg[0] == 'var':
            variavel, tipo = arg[1], arg[2]
            if variavel not in parser.regists:
                print(f"Erro p_escrita: variável '{variavel}' não declarada.")
                parser.success = False
                continue
            indice = parser.regists[variavel]
            cod.append(f'PUSHG {indice}')
            if tipo == 'integer' or tipo == 'boolean':
                cod.append('WRITEI')
            elif tipo == 'real':
                cod.append('WRITEF')
            elif tipo == 'string':
                cod.append('WRITES')
            else:
                print(f"Erro: tipo '{tipo}' não suportado em WRITELN.")
                parser.success = False

        elif isinstance(arg, tuple) and arg[0] == 'array_elem':
            access_cod = arg[1]
            tipo = arg[2]
            cod += access_cod
            if tipo == 'integer' or tipo == 'boolean':
                cod.append('WRITEI')
            elif tipo == 'real':
                cod.append('WRITEF')
            elif tipo == 'string':
                cod.append('WRITES')
            else:
                print(f"Erro: tipo de elemento '{tipo}' não suportado em WRITELN.")
                parser.success = False
        else:
            print(f"Erro: argumento inválido {arg} em WRITELN.")
            parser.success = False

    p[0] = cod + ['WRITELN'] # \n

def p_argumentos_escrita_vazio(p):
    """ 
    argumentos_escrita :
    """
    p[0] = []

def p_argumentos_escrita_one(p):
    """
    argumentos_escrita : argumento
    """
    p[0] = [p[1]]
       

def p_argumentos_escrita(p):
    """
    argumentos_escrita : argumentos_escrita VIRGULA argumento             
    """
    p[0] = p[1]+[p[3]]


def p_argumento_string(p):
    """
    argumento : STRING
    """
    p[0] = f'"{p[1]}"'

def p_argumento_identificador(p):
    """
    argumento : IDENTIFICADOR
    """
    variavel = p[1]
    if variavel not in parser.regists:
        print(f"Erro p_argumento_identificador: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = ('var', -1, None)
    else:
        tipo = parser.var_types[variavel]
        p[0] = ('var', variavel, tipo)

def p_argumento_array(p):
    """
    argumento : IDENTIFICADOR PRA expressao PRF
    """
    variavel = p[1]
    indice, tipo_indice = p[3]

    if variavel not in parser.regists or parser.var_types[variavel][0] != 'array':
        print(f"Erro: variável invalida.")
        parser.success = False
        p[0] = ('array_elem', [], 'integer') 
        return

    if tipo_indice != 'integer':
        print(f"Erro: índice de array '{variavel}' deve ser um inteiro.")
        parser.success = False

    var_info = parser.var_types[variavel]
    base_address = parser.regists[variavel]
    indice_inicio = var_info[1]
    tipo = var_info[3]

    cod = ["PUSHGP"] + indice + [f"PUSHI {indice_inicio - base_address}", "SUB", "LOADN"]

    p[0] = ('array_elem', cod, tipo)

# -------------------------------------------------
# Estruturas de controlo
# -------------------------------------------------
def p_if_h(p):
    """
    if_h : IF expressao THEN comando
         | IF expressao THEN comando ELSE comando
    """
    if len(p) == 5:
        expr, tipo_expr = p[2]
        if tipo_expr != 'boolean':
            print("Erro: expressão tem que ser condicional.")
            parser.success = False

        then_cod = p[4]
        fim = f"L{len(parser.labels)}"
        parser.labels.append(fim)

        p[0] = expr + [f"JZ {fim}"] + then_cod + [f"{fim}:"]
    else:
        expr, tipo_expr = p[2]
        if tipo_expr != 'boolean':
            print("Erro: expressão tem que ser condicional.")
            parser.success = False

        then_cod = p[4]
        else_cod = p[6]
        label_else = f"L{len(parser.labels)}"
        fim = f"L{len(parser.labels) + 1}"
        parser.labels += [label_else, fim]

        p[0] = (expr +[f"JZ {label_else}"] +then_cod +
            [f"JUMP {fim}", f"{label_else}:"] +else_cod +[f"{fim}:"]
        )

def p_while_h(p):
    """
    while_h : WHILE expressao DO comando
    """
    expr, tipo_expr = p[2]
    if tipo_expr != 'boolean':
        print("Erro: expressão tem que ser condicional.")
        parser.success = False

    inicio = f"Loop{len(parser.labels)}"
    fim = f"Loop{len(parser.labels) + 1}"
    parser.labels += [inicio, fim]

    cod = []
    cod.append(f"{inicio}:")
    cod += expr
    cod.append(f"JZ {fim}")
    cod += p[4]
    cod.append(f"JUMP {inicio}")
    cod.append(f"{fim}:")

    p[0] = cod
    

def p_for_h(p):
    """
    for_h : FOR IDENTIFICADOR ATRIBUICAO expressao TO  expressao DO comando
          | FOR IDENTIFICADOR ATRIBUICAO expressao DOWNTO  expressao DO comando
    """
    variavel = p[2]
    inicio_expr, tipo_inicio = p[4]
    fim_expr, tipo_fim = p[6]
    codigo = p[8]

    if variavel not in parser.regists:
        print(f"Erro p_for_h: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return

    if tipo_inicio != 'integer' or tipo_fim != 'integer':
        print("Erro: expressão de início/fim devem ser inteiros.")
        parser.success = False

    indice_variavel = parser.regists[variavel]
    inicio = f"Loop{len(parser.labels)}"
    fim = f"Loop{len(parser.labels)+1}"
    parser.labels += [inicio, fim]

    cod = []
    cod += inicio_expr
    cod.append(f"STOREG {indice_variavel}")
    cod.append(f"{inicio}:")
    cod.append(f"PUSHG {indice_variavel}")
    cod += fim_expr

    if p[5].lower() == "to": #crescente
        cod.append("INFEQ")
    elif p[5].lower() == "downto": #decrescente
        cod.append("SUPEQ")

    cod.append(f"JZ {fim}")
    cod += codigo
    cod.append(f"PUSHG {indice_variavel}")
    if p[5].lower() == "to":
        cod.append("PUSHI 1")
        cod.append("ADD")
    elif p[5].lower() == "downto":
        cod.append("PUSHI 1")
        cod.append("SUB")
    cod.append(f"STOREG {indice_variavel}")
    cod.append(f"JUMP {inicio}")
    cod.append(f"{fim}:")

    p[0] = cod



def p_chamada_funcao(p): # X(x,x,...)
    """
    chamada_funcao : IDENTIFICADOR PA lista_ids PF
    """
    for variavel in p[3]:
    #verificar se a variavel foi declarada anteriormente
        if variavel not in parser.regists: 
            print(f"Erro p_chamada_funcao: variável '{variavel}' não declarada.")
            parser.success = False
            p[0] = []
            return
        else:
            parser.parameters_f_input[variavel] = parser.var_types[variavel] 
    
    if len(parser.parameters_f_input) != len(parser.parameters_f_static):
        print(f"Erro p_chamada_funcao: Numero de parametros errados.")
        parser.success = False
        p[0] = []
        return
    
    if list(parser.parameters_f_input.values()) != list(parser.parameters_f_static.values()):
        print(f"Erro p_chamada_funcao: Tipos dos parametros errados.")
        parser.success = False
        p[0] = []
        return
    
    p[0] = p[1]


def p_chamada_funcao_vazio(p):  
    """
    chamada_funcao : IDENTIFICADOR PA PF
    """
    p[0] = p[1]


# -------------------------------------------------
# Expressões
# -------------------------------------------------
def p_expressao(p):
    """ 
    expressao : expressao MAIS expressao
              | expressao MENOS expressao
              | expressao VEZES expressao
              | expressao DIVIDIR expressao
              | expressao MAIOR expressao
              | expressao MENOR expressao
              | expressao IGUAL expressao
              | expressao DIFERENTE expressao
              | expressao MENOR_IGUAL expressao
              | expressao MAIOR_IGUAL expressao
              | expressao DIV expressao
              | expressao MOD expressao
              | expressao AND expressao
              | expressao OR expressao
    """
    ops = {'+': 'ADD', '-': 'SUB', '*': 'MUL', '/': 'DIV',
        '>': 'SUP', '<': 'INF', '=': 'EQUAL',
        '<=': 'INFEQ', '>=': 'SUPEQ',
        'div': 'DIV', 'mod': 'MOD',
        'and': 'AND', 'or': 'OR'}

    esq_cod, esq_tipo = p[1]
    dir_cod, dir_tipo = p[3]
    operacao = p[2].lower()

    def aux(cod): #verifica literal
        return (
            len(cod) == 1 and
            cod[0].startswith('PUSHS "') and
            len(cod[0]) == len('PUSHS "X"')
        )

    if operacao in ['=', '<>', '<', '>', '<=', '>=']:
        if aux(esq_cod):
            esq_cod += ["PUSHI 0", "CHARAT"]
            esq_tipo = 'integer'
        if aux(dir_cod):
            dir_cod += ["PUSHI 0", "CHARAT"]
            dir_tipo = 'integer'

    if operacao in ['+', '-', '*', '/', 'div', 'mod']:
        if esq_tipo != dir_tipo or esq_tipo != 'integer':
            print(f"Erro: operador '{operacao}' só é permitido entre inteiros.")
            parser.success = False
            p[0] = ([], None)
            return
        r_tipo = 'integer'
    elif operacao in ['and', 'or']:
        if esq_tipo != dir_tipo or esq_tipo != 'boolean':
            print(f"Erro: operador lógico '{operacao}' só funciona com booleanos.")
            parser.success = False
            p[0] = ([], None)
            return
        r_tipo = 'boolean'
    else: 
        if esq_tipo != dir_tipo:
            print(f"Erro: tipos incompatíveis em operação '{operacao}': {esq_tipo} e {dir_tipo}")
            parser.success = False
            p[0] = ([], None)
            return
        r_tipo = 'boolean'

    p[0] = (esq_cod + dir_cod + [ops[operacao]], r_tipo)

def p_expressao_parenteses(p):
    "expressao : PA expressao PF"
    p[0] = p[2]

def p_expressao_identificador(p):
    "expressao : IDENTIFICADOR"
    variavel = p[1]
    if variavel not in parser.regists:
            print(f"Erro p_expressao_identificador: variável '{variavel}' não declarada.")
            parser.success = False
            p[0] = ([], None)
    else:
        tipo = parser.var_types[variavel]
        p[0] = ([f"PUSHG {parser.regists[variavel]}"], tipo)

def p_expressao_indice(p):
    "expressao : IDENTIFICADOR PRA expressao PRF"
    nome = p[1]
    indice, _ = p[3]

    if nome not in parser.regists:
        print(f"Erro p_expressao_indice: variável '{nome}' não declarada.")
        parser.success = False
        p[0] = ([], None)
        return

    tipo_variavel = parser.var_types[nome]

    if isinstance(tipo_variavel, tuple) and tipo_variavel[0] == 'array':
        endr = parser.regists[nome]
        inicio = tipo_variavel[1]
        tipo = tipo_variavel[3]
        code = ["PUSHGP"] + indice + [f"PUSHI {inicio - endr}", "SUB", "LOADN"]
        p[0] = (code, tipo)

    elif tipo_variavel == 'string':
        endr = parser.regists[nome]
        code = [f"PUSHG {endr}"] + indice + ["PUSHI 1", "SUB", "CHARAT"]
        p[0] = (code, 'integer') 

    else:
        print(f"Erro: enderecos so em arrays ou strings.")
        parser.success = False
        p[0] = ([], None)

def p_expressao_inteiro(p):
    "expressao : INTEGER"
    p[0] = ([f"PUSHI {p[1]}"], 'integer')


def p_expressao_real(p):
    "expressao : REAL"
    p[0] = ([f"PUSHI {p[1]}"], 'real')

def p_expressao_string(p):
    "expressao : STRING"
    p[0] = ([f'PUSHS "{p[1]}"'], 'string')

def p_expressao_true(p):
    "expressao : TRUE"
    p[0] = (["PUSHI 1"], 'boolean')

def p_expressao_false(p):
    "expressao : FALSE"
    p[0] = (["PUSHI 0"], 'boolean')

def p_expressao_not(p):
    "expressao : NOT expressao"
    expr, tipo_expr = p[2]
    if tipo_expr != 'boolean':
        print("Erro: operador NOT só pode ser aplicado a booleanos.")
        parser.success = False
        p[0] = ([], None)
    else:
        p[0] = (expr + ["NOT"], 'boolean')

def p_expressao_length(p):
    "expressao : LENGTH PA IDENTIFICADOR PF"
    nome = p[3]
    if nome not in parser.regists:
        print(f"Erro: variável '{nome}' não declarada.")
        parser.success = False
        p[0] = ([], None)
        return

    tipo_variavel = parser.var_types[nome]
    if tipo_variavel != 'string' and (not isinstance(tipo_variavel, tuple) or tipo_variavel[0] != 'array'):
        print(f"Erro: LENGTH só pode ser usado com strings ou arrays. Tipo encontrado: {tipo_variavel}")
        parser.success = False
        p[0] = ([], None)
        return

    p[0] = ([f"PUSHG {parser.regists[nome]}", "STRLEN"], 'integer')
    
# -------------------------------------------------
# Declarações
# -------------------------------------------------
def p_declaracoes_vazio(p):
    """
    declaracoes :
    """
    p[0] = []

def p_declaracoes(p):
    """
    declaracoes : declaracoes declaracao_tipo 
    """
    p[0] = p[1] + p[2]  
    
def p_declaracao_tipo(p):
    """
    declaracao_tipo : dec_variaveis
                    | dec_funcao
    """ 
    p[0] = p[1]
    
def p_dec_variaveis(p):
    """
    dec_variaveis : VAR lista_vars
    """
    cod = []
    for variavel, tipo in p[2]:
        if tipo == "integer":
            cod.append("PUSHI 0")
        elif tipo == "string":
            cod.append('PUSHS ""')
        elif tipo == "boolean":
            cod.append("PUSHI 0")
        elif tipo == "array":
            try:
                inicio, fim, tipo = parser.var_types[variavel][1:]
                t = fim - inicio + 1
                for _ in range(t):
                    if tipo == "integer":
                        cod.append("PUSHI 0")
                    elif tipo == "string":
                        cod.append('PUSHS ""')
                    elif tipo == "boolean":
                        cod.append("PUSHI 0")
                    else:
                        raise ValueError(f"Tipo de elemento desconhecido '{tipo}' no array '{variavel}'")
            except KeyError:
                print(f"Erro: variável '{variavel}' não encontrada em var_types.")
                parser.success = False
        else:
            print(f"Erro: tipo desconhecido '{tipo}' para a variável '{variavel}'")
            parser.success = False
    if not parser.success:
        return
    
    p[0] = cod

def p_lista_vars_vazio(p):
    """
    lista_vars : 
    """
    p[0] = []

def p_lista_vars(p):
    """
    lista_vars : lista_vars lista_ids DOIS_PONTO tipo_variaveis PONTO_VIRGULA
    """
    tipo = p[4]
    res = []
    for nome in p[2]:
        if nome in parser.regists:
            print(f"Erro: a variável '{nome}' já foi declarada.")
            parser.success = False
        else:
            try:
                if isinstance(tipo, tuple) and tipo[0] == 'array':
                    inicio, fim, tipo_elem = tipo[1], tipo[2], tipo[3]
                    size = fim - inicio + 1
                    parser.regists[nome] = parser.index
                    parser.index += size
                    parser.var_types[nome] = ('array', inicio, fim, tipo_elem)
                    res.append((nome, 'array'))
                else:
                    parser.regists[nome] = parser.index
                    parser.index += 1
                    parser.var_types[nome] = tipo
                    res.append((nome, tipo))
                    
            except Exception as e:
                print(f"Erro ao processar variável '{nome}': {e}")
                parser.success = False
    p[0] = res
    
def p_lista_ids_unico(p):
    """
    lista_ids : IDENTIFICADOR
    """
    p[0] = [p[1]]
    
def p_lista_ids(p):
    """
    lista_ids : lista_ids VIRGULA IDENTIFICADOR
    """
    if p[3] in p[1]:
        print(f"Erro: variável '{p[3]}' duplicada na mesma declaração.")
        parser.success = False
    p[0] = p[1] + [p[3]]
    
def p_tipo_variaveis(p):
    """
    tipo_variaveis : INTEGER_TYPE
                   | REAL_TYPE
                   | BOOLEAN_TYPE
                   | CHAR_TYPE
                   | STRING_TYPE
                   | ARRAY PRA INTEGER PONTO_PONTO INTEGER PRF OF tipo_variaveis
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[3] > p[5]:
            print(f"Erro: limites inválidos para array: {p[3]}..{p[5]}")
            parser.success = False
            p[0] = None
        else:
            p[0] = ('array', p[3], p[5], p[8])

def p_dec_funcao(p):
    """
    dec_funcao : FUNCTION IDENTIFICADOR PA parametros PF DOIS_PONTO tipo_variaveis PONTO_VIRGULA dec_variaveis_f BEGIN bloco_comandos_f END PONTO_VIRGULA
    """

    tipo_id_a_retornar = parser.var_types_f[p[11][-1]] 
    cod = [] #codigo declaraçao de variaveis dos parametros da funcao
    
    for variavel, tipo in p[4]:
        if tipo == "integer":
            cod.append("PUSHI 0")
        elif tipo == "string":
            cod.append('PUSHS ""')
        elif tipo == "boolean":
            cod.append("PUSHI 0")
        elif tipo == "array":
            try:
                inicio, fim, tipo = parser.var_types_function_f[variavel][1:]
                t = fim - inicio + 1
                for _ in range(t):
                    if tipo == "integer":
                        cod.append("PUSHI 0")
                    elif tipo == "string":
                        cod.append('PUSHS ""')
                    elif tipo == "boolean":
                        cod.append("PUSHI 0")
                    else:
                        raise ValueError(f"Tipo de elemento desconhecido '{tipo}' no array '{variavel}'")
            except KeyError:
                print(f"Erro: variável '{variavel}' não encontrada em var_types_function.")
                parser.success = False
        else:
            print(f"Erro: tipo desconhecido '{tipo}' para a variável '{variavel}'")
            parser.success = False
    if not parser.success:
        return

    if p[2] != p[11][-2]:
        print(f"Erro: Identificador de return '{p[11][-2]}' não coincide com o nome da funçao '{p[2]}'.") 
        parser.success = False
        p[0] = []
    if tipo_id_a_retornar != p[7]:
        print(f"Erro: Funcao tem que retornar '{p[7]}', esta a retornar '{tipo_id_a_retornar}'.") 
        parser.success = False
        p[0] = []
    
    p[11].pop()
    p[11].pop()
    parser.labels_f[p[2]] = (p[7])
    p[0] = ["f"]+[p[2]+":"]+["PUSHFP"]+cod+p[9]+p[11]+[f"STOREL 0","PUSHL 0"]+["RETURN"]+["f"]

#=====================================
# TRATAMENTO FUNCOES
#=====================================
def p_if_h_f(p):
    """
    if_h_f : IF expressao THEN comando
         | IF expressao THEN comando ELSE comando
    """
    if len(p) == 5:
        expr, tipo_expr = p[2]
        if tipo_expr != 'boolean':
            print("Erro: expressão tem que ser condicional.")
            parser.success = False

        then_cod = p[4]
        fim = f"L{len(parser.labels_f)}"
        parser.labels_f.append(fim)

        p[0] = expr + [f"JZ {fim}"] + then_cod + [f"{fim}:"]
    else:
        expr, tipo_expr = p[2]
        if tipo_expr != 'boolean':
            print("Erro: expressão tem que ser condicional.")
            parser.success = False

        then_cod = p[4]
        else_cod = p[6]
        label_else = f"L{len(parser.labels_f)}"
        fim = f"L{len(parser.labels_f) + 1}"
        parser.labels_f += [label_else, fim]

        p[0] = (expr +[f"JZ {label_else}"] +then_cod +
            [f"JUMP {fim}", f"{label_else}:"] +else_cod +[f"{fim}:"]
        )

def p_while_h_f(p):
    """
    while_h_f : WHILE expressao DO comando
    """
    expr, tipo_expr = p[2]
    if tipo_expr != 'boolean':
        print("Erro: expressão tem que ser condicional.")
        parser.success = False

    inicio = f"Loop{len(parser.labels_f)}"
    fim = f"Loop{len(parser.labels_f) + 1}"
    parser.labels_f += [inicio, fim]

    cod = []
    cod.append(f"{inicio}:")
    cod += expr
    cod.append(f"JZ {fim}")
    cod += p[4]
    cod.append(f"JUMP {inicio}")
    cod.append(f"{fim}:")

    p[0] = cod
    

def p_for_h_f(p):
    """
    for_h_f : FOR IDENTIFICADOR ATRIBUICAO expressao TO  expressao DO comando
          | FOR IDENTIFICADOR ATRIBUICAO expressao DOWNTO  expressao DO comando
    """
    variavel = p[2]
    inicio_expr, tipo_inicio = p[4]
    fim_expr, tipo_fim = p[6]
    codigo = p[8]

    if variavel not in parser.regists_f:
        print(f"Erro p_for_h: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return

    if tipo_inicio != 'integer' or tipo_fim != 'integer':
        print("Erro: expressão de início/fim devem ser inteiros.")
        parser.success = False

    indice_variavel = parser.regists_f[variavel]
    inicio = f"Loop{len(parser.labels_f)}"
    fim = f"Loop{len(parser.labels_f)+1}"
    parser.labels_f += [inicio, fim]

    cod = []
    cod += inicio_expr
    cod.append(f"STOREG {indice_variavel}")
    cod.append(f"{inicio}:")
    cod.append(f"PUSHG {indice_variavel}")
    cod += fim_expr

    if p[5].lower() == "to": #crescente
        cod.append("INFEQ")
    elif p[5].lower() == "downto": #decrescente
        cod.append("SUPEQ")

    cod.append(f"JZ {fim}")
    cod += codigo
    cod.append(f"PUSHG {indice_variavel}")
    if p[5].lower() == "to":
        cod.append("PUSHI 1")
        cod.append("ADD")
    elif p[5].lower() == "downto":
        cod.append("PUSHI 1")
        cod.append("SUB")
    cod.append(f"STOREG {indice_variavel}")
    cod.append(f"JUMP {inicio}")
    cod.append(f"{fim}:")

    p[0] = cod


def p_bloco_comandos_vazio_f(p):
    """
    bloco_comandos_f :
    """
    p[0] = []

def p_bloco_comandos_f(p):
    """
    bloco_comandos_f : lista_comandos_pv_f comando_f
                   | lista_comandos_pv_f
                   | comando_f
    """
    if len(p) == 3:
        p[0] = p[1]+p[2]
    else:
        p[0] = p[1]


def p_lista_comandos_one_f(p):
    """ 
    lista_comandos_pv_f : comando_pv_f
    """
    p[0] = p[1]

def p_lista_comandos_f(p):
    """
    lista_comandos_pv_f : lista_comandos_pv_f comando_pv_f
    """
    p[0] = p[1] + p[2]
    

# -------------------------------------------------
# Comandos
# -------------------------------------------------
def p_comando_pv_f(p):
    """
    comando_pv_f : comando_f PONTO_VIRGULA
    """
    p[0] = p[1]

def p_comando_f(p):
    """
    comando_f : atribuicao_f
            | leitura_f
            | escrita_f
            | if_h_f
            | while_h_f
            | for_h_f
            | return_f
            | BEGIN bloco_comandos_f END
    """
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]


# verificar identificador existe
def p_atribuicao_funcao_f(p):
    """
    atribuicao_f : IDENTIFICADOR ATRIBUICAO chamada_funcao
    """
    variavel = p[1]

    tipo_fun = parser.labels_f[p[3]]

    #verificar se a variavel foi declarada anteriormente
    if variavel not in parser.regists_f: 
        print(f"Erro p_atribuicao_funcao_f: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return
    #resgata tipo da variavel 
    tipo_var = parser.var_types_f[variavel]

    if tipo_var != tipo_fun:
        print(f"Erro: atribuição incompatível. Variável '{variavel}' é '{tipo_var}' mas funcao retorna um '{tipo_fun}'.")
        parser.success = False

    p[0] = [f"PUSHA {p[3]}","CALL"]+[f"STOREG {parser.regists_f[variavel]}"]

def p_atribuicao_f(p):
    'atribuicao_f : IDENTIFICADOR ATRIBUICAO expressao'
    variavel = p[1]
    expr, tipo_expr = p[3]
    
    #verificar se a variavel foi declarada anteriormente
    if variavel not in parser.regists_f: 
        print(f"Erro p_atribuicao_f: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return
    
    #resgata tipo da variavel 
    tipo = parser.var_types_f[variavel]

    if tipo != tipo_expr:
        print(f"Erro: atribuição incompatível. Variável '{variavel}' é '{tipo}' mas expressão é '{tipo_expr}'.")
        parser.success = False

    p[0] = expr + [f"STOREG {parser.regists_f[variavel]+2}"] 


def p_leitura_f(p):
    """
    leitura_f : READLN PA IDENTIFICADOR PF
            | READLN PA IDENTIFICADOR PRA expressao PRF PF
    """
    if len(p) == 5:
        argumento = p[3]
        #verifica se foi declarado anteriormente
        if argumento not in parser.regists_f:
            print(f"Erro p_leitura: variável '{argumento}' não declarada.")
            parser.sucess = False
            p[0] = []
            return

        indice = parser.regists_f[argumento]
        tipo = parser.var_types_f[argumento]

        cod_traducao = ["READ"]
        if tipo == "integer" or tipo == "boolean":
            cod_traducao.append("ATOI") #tranforma string em inteiro
        elif tipo == "real":
            cod_traducao.append("ATOF") #tranforma string em float
        elif tipo == "string":
            pass    
        else:
            print(f"Erro: tipo '{tipo}' não suportado em READLN.")
            parser.success = False

        cod_traducao.append(f"STOREG {indice}")
        p[0] = cod_traducao
    else:
        argumento = p[3]
        expr, tipo_expr = p[5]

        if argumento not in parser.regists or parser.var_types[argumento][0] != 'array':
            print(f"Erro p_leitura: variável '{argumento}' não é um array ou não foi declarada.")
            parser.success = False
            return

        if tipo_expr != 'integer':
            print(f"Erro: índice de array '{argumento}' deve ser do tipo integer.")
            parser.success = False

        start = parser.var_types_f[argumento][1]
        base_address = parser.regists_f[argumento]
        element_type = parser.var_types_f[argumento][3]

        cod_traducao = ["PUSHGP"] + expr + [f"PUSHI {start - base_address}", "SUB", "READ"]

        if element_type == "integer" or element_type == "boolean":
            cod_traducao.append("ATOI")
        elif element_type == "real":
            cod_traducao.append("ATOF")
        elif element_type == "string":
            pass
        else:
            print(f"Erro: tipo de elemento desconhecido '{element_type}' no array '{argumento}'.")
            parser.success = False
            return

        cod_traducao.append("STOREN")
        p[0] = cod_traducao


def p_escrita_f(p):
    'escrita_f : WRITELN PA argumentos_escrita_f PF'
    cod = []
    for arg in p[3]:
        if isinstance(arg, str) and arg.startswith('"'):
            cod.append(f'PUSHS {arg}')
            cod.append('WRITES')

        elif isinstance(arg, tuple) and arg[0] == 'var':
            variavel, tipo = arg[1], arg[2]
            if variavel not in parser.regists_f:
                print(f"Erro p_escrita_f: variável '{variavel}' não declarada.")
                parser.success = False
                continue
            indice = parser.regists_f[variavel]
            cod.append(f'PUSHG {indice}')
            if tipo == 'integer' or tipo == 'boolean':
                cod.append('WRITEI')
            elif tipo == 'real':
                cod.append('WRITEF')
            elif tipo == 'string':
                cod.append('WRITES')
            else:
                print(f"Erro: tipo '{tipo}' não suportado em WRITELN.")
                parser.success = False

        elif isinstance(arg, tuple) and arg[0] == 'array_elem':
            access_cod = arg[1]
            tipo = arg[2]
            cod += access_cod
            if tipo == 'integer' or tipo == 'boolean':
                cod.append('WRITEI')
            elif tipo == 'real':
                cod.append('WRITEF')
            elif tipo == 'string':
                cod.append('WRITES')
            else:
                print(f"Erro: tipo de elemento '{tipo}' não suportado em WRITELN.")
                parser.success = False
        else:
            print(f"Erro: argumento inválido {arg} em WRITELN.")
            parser.success = False

    p[0] = cod + ['WRITELN'] # \n

def p_argumentos_escrita_vazio_f(p):
    """ 
    argumentos_escrita_f :
    """
    p[0] = []

def p_argumentos_escrita_one_f(p):
    """
    argumentos_escrita_f : argumento_f
    """ 
    p[0] = [p[1]]
       

def p_argumentos_escrita_f(p):
    """
    argumentos_escrita_f : argumentos_escrita_f VIRGULA argumento             
    """
    p[0] = p[1]+[p[3]]


def p_argumento_string_f(p):
    """
    argumento_f : STRING
    """
    p[0] = f'"{p[1]}"'

def p_argumento_identificador_f(p):
    """
    argumento_f : IDENTIFICADOR
    """
    variavel = p[1]
    if variavel not in parser.regists_f:
        print(f"Erro p_argumento_identificador_f: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = ('var', -1, None)
    else:
        tipo = parser.var_types_f[variavel]
        p[0] = ('var', variavel, tipo)

def p_argumento_array_f(p):
    """
    argumento_f : IDENTIFICADOR PRA expressao PRF
    """
    variavel = p[1]
    indice, tipo_indice = p[3]

    if variavel not in parser.regists_f or parser.var_types_f[variavel][0] != 'array':
        print(f"Erro: variável invalida.")
        parser.success = False
        p[0] = ('array_elem', [], 'integer') 
        return

    if tipo_indice != 'integer':
        print(f"Erro: índice de array '{variavel}' deve ser um inteiro.")
        parser.success = False

    var_info = parser.var_types_f[variavel]
    base_address = parser.regists_f[variavel]
    indice_inicio = var_info[1]
    tipo = var_info[3]

    cod = ["PUSHGP"] + indice + [f"PUSHI {indice_inicio - base_address}", "SUB", "LOADN"]

    p[0] = ('array_elem', cod, tipo)


def p_dec_variaveis_f_vazio(p):
    """
    dec_variaveis_f :
    """
    p[0] = []

def p_dec_variaveis_f(p):
    """
    dec_variaveis_f : VAR lista_vars_f
    """
    cod = []
    for variavel, tipo in p[2]:
        if tipo == "integer":
            cod.append("PUSHI 0")
        elif tipo == "string":
            cod.append('PUSHS ""')
        elif tipo == "boolean":
            cod.append("PUSHI 0")
        elif tipo == "array":
            try:
                inicio, fim, tipo = parser.var_types_f[variavel][1:]
                t = fim - inicio + 1
                for _ in range(t):
                    if tipo == "integer":
                        cod.append("PUSHI 0")
                    elif tipo == "string":
                        cod.append('PUSHS ""')
                    elif tipo == "boolean":
                        cod.append("PUSHI 0")
                    else:
                        raise ValueError(f"Tipo de elemento desconhecido '{tipo}' no array '{variavel}'")
            except KeyError:
                print(f"Erro: variável '{variavel}' não encontrada em var_types.")
                parser.success = False
        else:
            print(f"Erro: tipo desconhecido '{tipo}' para a variável '{variavel}'")
            parser.success = False
    if not parser.success:
        return
    p[0] = cod

def p_lista_vars_vazio_f(p):
    """
    lista_vars_f : 
    """
    p[0] = []

def p_lista_vars_f(p):
    """
    lista_vars_f : lista_vars_f lista_ids_f DOIS_PONTO tipo_variaveis PONTO_VIRGULA
    """
    tipo = p[4]
    res = []
    for nome in p[2]:

        if nome in parser.regists_f:
            print(f"Erro p_lista_vars_f: a variável '{nome}' já foi declarada.")
            parser.success = False
        else:
            try:
                if isinstance(tipo, tuple) and tipo[0] == 'array':
                    inicio, fim, tipo_elem = tipo[1], tipo[2], tipo[3]
                    size = fim - inicio + 1
                    parser.regists_f[nome] = parser.index_f
                    parser.index_f += size
                    parser.var_types_f[nome] = ('array', inicio, fim, tipo_elem)
                    res.append((nome, 'array'))
                else:
                    parser.regists_f[nome] = parser.index_f
                    parser.index_f += 1
                    parser.var_types_f[nome] = tipo
                    res.append((nome, tipo))
            except Exception as e:
                print(f"Erro ao processar variável '{nome}': {e}")
                parser.success = False

    p[0] = res


def p_lista_ids_unico_f(p):
    """
    lista_ids_f : IDENTIFICADOR
    """
    p[0] = [p[1]]
    
def p_lista_ids_f(p):
    """
    lista_ids_f : lista_ids_f VIRGULA IDENTIFICADOR
    """
    if p[3] in p[1]:
        print(f"Erro: variável '{p[3]}' duplicada na mesma declaração.")
        parser.success = False
    p[0] = p[1] + [p[3]]

def p_return_f(p):
    """
    return_f : IDENTIFICADOR ATRIBUICAO IDENTIFICADOR PONTO_VIRGULA
    """
    variavel = p[3]
    
    #verificar se a variavel foi declarada anteriormente
    if variavel not in parser.regists_f: 
        print(f"Erro p_return: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return
    p[0] = [p[1]]+[p[3]]

def p_parametros_vazio(p):
    """ 
    parametros :
    """
    p[0] = []

def p_parametros_unico(p):
    """
    parametros : parametro
    """
    p[0] = p[1]

def p_parametros(p):
    """
    parametros : parametro PONTO_VIRGULA parametros
    """
    p[0] = p[1] + p[3]
    

def p_parametro_vazio(p):
    """
    parametro :
    """
    p[0] = []

def p_parametro(p):
    """
    parametro : parametro lista_ids DOIS_PONTO tipo_variaveis 
    """

    tipo = p[4]
    res = []
    for nome in p[2]:
        if nome in parser.regists_f:
            print(f"Erro p_parametro: a variável '{nome}' já foi declarada.")
            parser.success = False
            return
        else:
            try:
                parser.regists_f[nome] = parser.index_f
                parser.index_f += 1
                parser.var_types_f[nome] = tipo
                res.append((nome, tipo))
                parser.parameters_f_static[nome] = tipo   
            
            except Exception as e:
                print(f"Erro ao processar variável '{nome}': {e}")
                parser.success = False
                return
    p[0] = res # res = [('var', 'tipo'), ('var', 'tipo')]

def p_return(p):
    """
    return : IDENTIFICADOR ATRIBUICAO IDENTIFICADOR PONTO_VIRGULA
    """
    variavel = p[3]
    
    #verificar se a variavel foi declarada anteriormente
    if variavel not in parser.regists: 
        print(f"Erro p_return: variável '{variavel}' não declarada.")
        parser.success = False
        p[0] = []
        return
    
    p[0] = [p[1]]+[p[3]]

# -------------------------------------------------
# Erros
# -------------------------------------------------
def p_error(p):
    if p:
        print(f"Erro de sintaxe próximo a '{p.value}' na linha {p.lineno}.")
    else:
        print("Erro de sintaxe.")
    parser.success = False

# -------------------------------------------------
# Parser
# -------------------------------------------------
parser = yacc.yacc()

parser.index = 0
parser.regists = {}
parser.var_types = {}
parser.labels = []


parser.index_f = 0
parser.regists_f = {}
parser.parameters_f_input = {}
parser.parameters_f_static = {}
parser.var_types_f = {}
parser.labels_f = {}

import sys
try:

    program_input = sys.stdin.read()

    parser.success = True
    parser.parse(program_input)
    if parser.success:
        print("\nPrograma: \n", program_input)
except Exception as e:
    print(f"Erro ao processar: {e}")
    parser.success = False
