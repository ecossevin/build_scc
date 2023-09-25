from loki import (
    Sourcefile, FindNodes, CallStatement, 
    Transformer, Dimension, ir, 
    Scalar, Assignment, fgen,
    FindVariables, symbols, demote_variables,
    Intrinsic, Variable, SymbolAttributes,
    DerivedType, VariableDeclaration, flatten
)

from loki.transform import resolve_associates

import os
import sys

import logical
import ResolveVector
import ExplicitArraySyntaxes


def load_subroutine(path, file, name):
    source=Sourcefile.from_file(path+file)
    return(source[name])

def save_subroutine(path, file):
    from pathlib import Path
    Sourcefile.to_file(fgen(routine), Path(path+name+'.F90'))




def add_openacc1(routine):
    call_lst=[]
    call_map={}
    suffix='_OPENACC'
    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)

    for call in FindNodes(CallStatement).visit(routine.body):
        if call.name != "DR_HOOK":
            call_lst.append(call.name.name)
            new_call=call.clone(name=call.name, arguments=call.arguments)
            new_call._update(name=call.name.clone(name=f'{call.name}{suffix}'))
            new_call._update(kwarguments=new_call.kwarguments + ((stack_argument.name, stack_local),))
            call_map.update({call : new_call})

    routine_importSPEC=FindNodes(ir.Import).visit(routine.spec)
    imp_map={}
    for imp in routine_importSPEC:
        name=imp.module.replace(".intfb.h","").upper()
        if imp.c_import==True and any(call==name for call in call_lst):
            new_name=imp.module.replace("intfb.f","")+"_openacc"+".intfb.h"
            new_imp=imp.clone(module=new_name)
            imp_map.update({imp : new_imp})
    routine.body=Transformer(call_map).visit(routine.body)
    routine.spec=Transformer(imp_map).visit(routine.spec)


def add_openacc2(routine):
    call_lst=[]
    suffix='_OPENACC'
    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)
    for call in FindNodes(CallStatement).visit(routine.body):
        call_lst.append(call.name.name)
        call.name.name=call.name.name+suffix
        call._update(kwarguments=call.kwarguments + ((stack_argument.name, stack_local),))

    routine_importSPEC=FindNodes(ir.Import).visit(routine.spec)
    for imp in routine_importSPEC:
        name=imp.module.replace("intffb.h","").upper()
        if imp.c_import==True and any(call==name for call in call_lst):
            imp.module=imp.module.replace(".intfb.h","")+"_openacc"+".intfb.h"

def remove_loop(routine):
    loop_map={}
    for loop in FindNodes(ir.Loop).visit(routine.body):
        if loop.variable in horizontal_lst:
            loop_map[loop]=loop.body
    #routine_new.body=Transformer(loop_map).visit(routine.body)
    routine.body=Transformer(loop_map).visit(routine.body)

def rename(routine):
    routine.name=routine.name+'_OPENACC'

def acc_seq(routine):
    routine.spec.insert(0,ir.Pragma(keyword='acc', content='routine ('+routine.name+') seq'))
    routine.spec.insert(1,ir.Comment(text=''))

def jlon_kidia(routine, end_index, begin_index, newrange):
    jlon=Scalar("JLON")
#    kidia=Scalar(begin_index)
    routine.spec.append(Intrinsic("JLON="+begin_index.name))
#    routine.spec.append(Assignment(jlon, kidia))

def stack_mod(routine):
    idx=0
    for spec in routine.spec.body:
        if type(spec)==ir.Intrinsic and spec.text=='IMPLICIT NONE':
            break
        idx=idx+1
    routine.spec.insert(idx, ir.Import(module='STACK_MOD'))
    routine.spec.insert(idx+1, ir.Import(module='stack.h', c_import=True))
    routine.spec.insert(idx+2, ir.Comment(text=''))

def rm_KLON(routine, horizontal):
    routine_arg=[var.name for var in routine.arguments]
    to_demote=FindVariables(unique=True).visit(routine.spec)
    to_demote=[var for var in to_demote if isinstance(var, symbols.Array)]
    to_demote=[var for var in to_demote if var.shape[-1] == horizontal.size]
    #to_demote=[var for var in to_demote if var.shape[0] == horizontal.size]
            #to_demote=[var for var in to_demote if var.shape[-1] == horizontal.size and var.shape[0] == horizontal.size]
#    if True :
#        to_demote = [var for var in to_demote if var.name not in routine_arg]

#    calls = FindNodes(ir.CallStatement).visit(routine.body)
#    call_args = flatten(call.arguments for call in calls)
#    call_args += flatten(list(dict(call.kwarguments).values()) for call in calls)
#    to_demote = [v for v in to_demote if v.name not in call_args]

    var_names=tuple(var.name for var in to_demote)
    if var_names:
        demote_variables(routine, variable_names=('TOTO',), dimensions='KLON')
###    if var_names:
###    #transform_array_indexing.demote_variables(routine, var_names, dimensions=horizontal.size)
###        print(routine.to_fortran())
###        print("names=",var_names)
###        print("typenamess=",type(var_names))
###        print("typenamess0=",type(var_names[0]))
###
###
###
###        print("**************************")
###        print("second routine")
###
###        file='src2/toto2.F90'
###        source=Sourcefile.from_file(file)
###        routine2=source.subroutines[0]
###        print(routine2.to_fortran())
###        demote_variables(routine2, variable_names=('TOTO',), dimensions='KLON')
###        print(routine2.to_fortran())
###        print("**************************")
###
###        print("**************************")
###        print("first routine")
###        print(routine.to_fortran())
###        demote_variables(routine, variable_names=('TOTO',), dimensions='KLON')
###        print(routine.to_fortran())
###        print("**************************")

def alloc_temp(routine):
    routine_arg=[var.name for var in routine.arguments]
#    variables=FindVariables(unique=False).visit(routine.spec)
#    variables=[var for variables is instance(var, symbols.Array)]
#    variabes=[var for var in variables if var.name not in routine_arg]
    

    temp_map={}
    for decls in FindNodes(VariableDeclaration).visit(routine.spec):
        intrinsic_lst=[] #intrinsic lst for temp macro var decl
        var_lst=[] #var to keep in the decl.
        for s in decls.symbols:
            if isinstance(s, symbols.Array):
                if s.name not in routine_arg:
                    if s.type.kind:
                        new_s='temp ('+s.type.dtype.name+' (KIND='+s.type.kind.name+'), '+s.name+', ('
                    else:
                        new_s='temp ('+s.type.dtype.name+', '+s.name+', ('
                    for shape in s.shape:
                        new_s=new_s+str(shape)+', '
                    new_s=new_s[:-2]
                    new_s=new_s+'))'
                    alloc='alloc ('+s.name+')'
                    routine.spec.append(Intrinsic(alloc))
                    intrinsic_lst.append(Intrinsic(new_s))
                else: #if array in routine args
#                    var_lst.append(decls.clone(symbols=s))
                    var_lst=[s]
                    VAR=decls.clone(symbols=var_lst)
                    intrinsic_lst.append(VAR)
            else: #if not an array
#                var_lst.append(decls.clone(symbols=s))
                var_lst=[s]
                VAR=decls.clone(symbols=var_lst)
                intrinsic_lst.append(VAR)

        temp_map[decls]=tuple(intrinsic_lst)
#        VAR=decls.clone(symbols=var_lst)
#        intrinsic_lst.append(VAR)

    routine.spec=Transformer(temp_map).visit(routine.spec)
    



def ystack(routine):
    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)

    routine.arguments+=(stack_argument,)
    routine.variables+=(stack_argument, stack_local,)
    routine.spec.append(Assignment(stack_local, stack_argument))



#pathR='src/phys_dmn/'
#pathW='src/phys_dmn_loki/'
###file='cucalln_mf.F90'
###name='CUCALLN_MF'
#file='actke.F90'
#fileR=file
#fileW=file
#name='ACTKE'
#
##pathtest='test/'
##filetest='logic.F90'
##nametest='LOGIC'
##pathR=pathtest
##pathW=pathtest
##fileR=filetest
##fileW=filetest+'_loki.F90'
##name=nametest

file=sys.argv[1]
#routine=load_subroutine(pathR, fileR, name)
source=Sourcefile.from_file(file)
routine=source.subroutines[0]


horizontal=Dimension(name='horizontal',size='KLON',index='JLON',bounds=['KIDIA','KFIDIA'],aliases=['NPROMA','KDIM%KLON','D%INIT'])
horizontal_lst=['JLON', 'JL']
vertical=Dimension(name='vertical',size='KLEV',index='JLEV')
true_symbols=[]
false_symbols=['LHOOK', 'LMUSCLFA','LFLEXDIA']

logical.transform_subroutine(routine, true_symbols, false_symbols)
end_index, begin_index, newrange=ExplicitArraySyntaxes.ExplicitArraySyntaxes(routine, horizontal, horizontal_lst)
loop_variable=Scalar("JLON")
bounds=[begin_index, end_index]
print(bounds)
##add_openacc1(routine)
add_openacc2(routine)
remove_loop(routine)
rename(routine)
acc_seq(routine)
stack_mod(routine)
jlon_kidia(routine, end_index, begin_index, newrange) #at the end
rm_KLON(routine, horizontal)
ResolveVector.resolve_vector_dimension(routine, loop_variable, bounds)
resolve_associates(routine)
###
ystack(routine)
alloc_temp(routine)
jlon_kidia(routine, end_index, begin_index, newrange) #at the end
##save_subroutine(pathW, file)
from pathlib import Path
Sourcefile.to_file(fgen(routine), Path('loki/'+file))
