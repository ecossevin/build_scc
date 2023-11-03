from loki import (
    Sourcefile, FindNodes, CallStatement, 
    Transformer, Dimension, ir, 
    Scalar, Assignment, fgen,
    FindVariables, symbols, demote_variables,
    Intrinsic, Variable, SymbolAttributes,
    DerivedType, VariableDeclaration, flatten,
    BasicType, FindInlineCalls, SubstituteExpressions,
    Nullify
)

from loki.transform import resolve_associates

import os
import sys

from termcolor import colored
import logical
import ResolveVector
import ExplicitArraySyntaxes

import re


def load_subroutine(path, file, name):
    source=Sourcefile.from_file(path+file)
    return(source[name])

def save_subroutine(path, file):
    from pathlib import Path
    Sourcefile.to_file(fgen(routine), Path(path+name+'.F90'))




#def add_openacc1(routine):
#    call_lst=[]
#    call_map={}
#    suffix='_OPENACC'
#    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
#    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)
#
#    for call in FindNodes(CallStatement).visit(routine.body):
#        print("call.name=",call.name)
#        if call.name == "ABOR1":
#    #        call_lst.append(call.name.name) don't add in call_lst, no #include abor1_acc
#            new_call=call.clone(name=call.name, arguments=call.arguments)
#            new_call._update(name=call.name.clone(name=call.name+"_ACC"))
#            call_map.update({call : new_call})
#        elif call.name != "DR_HOOK":
#            call_lst.append(call.name.name)
#            new_call=call.clone(name=call.name, arguments=call.arguments)
#            new_call._update(name=call.name.clone(name=f'{call.name}{suffix}'))
#            new_call._update(kwarguments=new_call.kwarguments + ((stack_argument.name, stack_local),))
#            call_map.update({call : new_call})
#
#    routine_importSPEC=FindNodes(ir.Import).visit(routine.spec)
#    imp_map={}
#    for imp in routine_importSPEC:
#        name=imp.module.replace(".intfb.h","").upper()
#        if imp.c_import==True and any(call==name for call in call_lst):
#            new_name=imp.module.replace("intfb.f","")+"_openacc"+".intfb.h"
#            new_imp=imp.clone(module=new_name)
#            imp_map.update({imp : new_imp})
#    routine.body=Transformer(call_map).visit(routine.body)
#    routine.spec=Transformer(imp_map).visit(routine.spec)


def add_openacc2(routine):
    call_lst=[]
    suffix='_OPENACC'
    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)
    for call in FindNodes(CallStatement).visit(routine.body):
        if call.name == "ABOR1":
            
        #call_lst.append(call.name.name)
            call.name.name=call.name.name+"_ACC"
#            call._update(kwarguments=call.kwarguments + ((stack_argument.name, stack_local),))

        elif call.name != "DR_HOOK":
            call_lst.append(call.name.name)
            call.name.name=call.name.name+suffix
            call._update(kwarguments=call.kwarguments + ((stack_argument.name, stack_local),))

    routine_importSPEC=FindNodes(ir.Import).visit(routine.spec)
    for imp in routine_importSPEC:
        name=imp.module.replace(".intfb.h","").upper()
#        print("name=",name)
#        print("call_lst=",call_lst)
        if imp.c_import==True and any(call==name for call in call_lst):
 #       # if any(call==name for call in call_lst):
 #            print("imp.module=",imp.module)
 #            print("imp.module.type=",type(imp.module))

            new_name=imp.module.replace(".intfb.h","")+"_openacc"+".intfb.h"
            imp._update(module=f'{new_name}')
#            #print("imp.module_new=",imp.module)
#            print("new_name=", new_name)
#            print("imp.module_new",imp.module)

def remove_loop(routine):
    loop_map={}
    for loop in FindNodes(ir.Loop).visit(routine.body):
        if loop.variable in horizontal_lst:
            loop_map[loop]=loop.body
    routine.body=Transformer(loop_map).visit(routine.body)

def rename(routine):
    routine.name=routine.name+'_OPENACC'

def acc_seq(routine):
    routine.spec.insert(0,ir.Pragma(keyword='acc', content='routine ('+routine.name+') seq'))
    routine.spec.insert(1,ir.Comment(text=''))

def jlon_kidia(routine, end_index, begin_index, new_range, loop_variable):
    
    routine.spec.append(Assignment(loop_variable, begin_index))
#    kidia=Scalar(begin_index)
#    routine.spec.append(Assignment(jlon, kidia))

def stack_mod(routine):
    idx=-1
    for spec in routine.spec.body:
#        if type(spec)==ir.Intrinsic and spec.text=='IMPLICIT NONE':
#            break
        if type(spec)==ir.VariableDeclaration:
            break
        idx=idx+1
    routine.spec.insert(idx-1, ir.Import(module='STACK_MOD'))
    routine.spec.insert(idx, ir.Import(module='stack.h', c_import=True))
    routine.spec.insert(idx+1, ir.Comment(text=''))

def rm_KLON(routine, horizontal, horizontal_size):
    demote_arg=False #rm KLON in function arg if True
    routine_arg=[var.name for var in routine.arguments]
    to_demote=FindVariables(unique=True).visit(routine.spec)
    to_demote=[var for var in to_demote if isinstance(var, symbols.Array)]
    to_demote=[var for var in to_demote if var.shape[-1] == horizontal_size]
    #to_demote=[var for var in to_demote if var.shape[0] == horizontal.size]
            #to_demote=[var for var in to_demote if var.shape[-1] == horizontal.size and var.shape[0] == horizontal.size]
    if not demote_arg :
        to_demote = [var for var in to_demote if var.name not in routine_arg]


        calls = FindNodes(ir.CallStatement).visit(routine.body)
        call_args = flatten(call.arguments for call in calls)
        call_args += flatten(list(dict(call.kwarguments).values()) for call in calls)
        to_demote = [v for v in to_demote if v.name not in call_args]

    var_names=tuple(var.name for var in to_demote)
    if var_names:
       # demote_variables(routine, variable_names=var_names, dimensions='KLON')
        demote_variables(routine, var_names, dimensions=horizontal_size)

def alloc_temp(routine):
    routine_arg=[var.name for var in routine.arguments]
    

    temp_map={}
    for decls in FindNodes(VariableDeclaration).visit(routine.spec):
        intrinsic_lst=[] #intrinsic lst for temp macro var decl
        var_lst=[] #var to keep in the decl.
        for s in decls.symbols:
            if isinstance(s, symbols.Array):
                if not s.type.pointer:
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
#                        var_lst.append(decls.clone(symbols=s))
                        var_lst=[s]
                        VAR=decls.clone(symbols=var_lst)
                        intrinsic_lst.append(VAR)
                else: #if s is a pointer
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
    

def get_horizontal(routine, horizontal_size_lst):
    verbose=True
    for name in horizontal_size_lst:
        if name in routine.variable_map:
            if verbose: print("horizontal size = ",name)
            return(name)
    for var in FindVariables().visit(routine.body):
#        print(var)
        for vvar in var.name.split("%"):
            if vvar in horizontal_size_lst:
                if verbose: print("horizontal size = ",name)
                return(var.name)
    print(colored("Horizontal size not found in routine args!", "red"))


def ystack1(routine):
#    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK")),scope=routine)
    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)

    routine.arguments+=(stack_argument,)

def ystack2(routine):
#    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK")),scope=routine)
    stack_argument=Variable(name="YDSTACK", type=SymbolAttributes(DerivedType(name="STACK"), intent='in'))
    stack_local=Variable(name="YLSTACK", type=SymbolAttributes(DerivedType(name="STACK")), scope=routine)

    routine.variables+=(stack_argument, stack_local,)
    routine.spec.append(Assignment(stack_local, stack_argument))


def get_loop_variable(routine, horizontal_lst):
    var_lst=FindVariables(unique=True).visit(routine.spec)
    var_lst=[var for var in var_lst if var.name in horizontal_lst]
    if var_lst : 
        loop_variable=var_lst[0]
    else:
        loop_variable=Scalar("JLON") #Scalar(horizontal_lst[0])
        jlon=Variable(name="JLON", type=SymbolAttributes(BasicType.INTEGER, kind=Variable(name='JPIM')))
        routine.variables+=(jlon,)

    return(loop_variable)

def rm_sum(routine):
    call_map={}
    for assign in FindNodes(Assignment).visit(routine.body):
        for call in FindInlineCalls().visit(assign):
            if (call.name=="SUM"):
                call_map[call]=call.parameters[0]
    if call_map:
        routine.body=SubstituteExpressions(call_map).visit(routine.body)

def generate_interface(routine):
    removal_map={}
    imports = FindNodes(ir.Import).visit(routine.spec)
    routine_new=routine.clone()
    for im in imports:
        if im.c_import==True:
            removal_map[im]=None
    routine_new.spec = Transformer(removal_map).visit(routine_new.spec)
    Sourcefile.to_file(fgen(routine_new.interface), Path(pathW+".intfb.h"))

def write_print(routine):
    intr_map={}
    for intr in FindNodes(Intrinsic).visit(routine.body):
        if "WRITE" in intr.text:
            pattern='WRITE\(NULERR, \*\)(.*)'
            match=re.search(pattern,intr.text)
            string=match.group(1)
            new_intr='PRINT *, '+string
            intr_map[intr]=Intrinsic(new_intr)
    routine.body=Transformer(intr_map).visit(routine.body)
            
#---------------------------------------------------------
#---------------------------------------------------------
#Pointers
#---------------------------------------------------------
#---------------------------------------------------------

def find_pt(routine):
    tmp_pt=[]
    tmp_target=[]
    #for var in FindVariables().visit(routine.variables):
    routine_args=[var.name for var in routine.arguments]
    for decls in FindNodes(VariableDeclaration).visit(routine.spec):
        for s in decls.symbols:
            if s.name not in routine_args:
                if s.type.pointer:
                    tmp_pt.append(s)
            if s.type.target:
                tmp_target.append(s)
    return(tmp_pt, tmp_target)
             
             
def get_dim_pt(routine, tmp_pt, horizontal_size):
    assign_map={}
    tmp_pt_klon={}
    for assign in FindNodes(Assignment).visit(routine.body):
        assigned=False
        is_target=False
        is_pt=False
        rhs=FindVariables().visit(assign.rhs)
        rhs=list(rhs)
        lhs=FindVariables().visit(assign.lhs)
        lhs=list(lhs)
        if (len(rhs)==1 and len(lhs)==1):
            for pt in tmp_pt:
                if pt.name==lhs[0].name:
                   is_pt=True
                   break
            for target in tmp_target:
                if target.name==rhs[0].name:
                   is_target=True
                   break
            if is_pt and is_target:
                if target.dimensions[0]=='KPROMA': #replace KPROMA by hor_dim... here we assume that if they are no dim, then the dim isn't NPROMA.... 
                    tmp_pt_klon[lhs[0].name]=rhs[0]
                    new_assign='assoc ('+lhs[0].name+','+rhs[0].name+')'
                    assign_map[assign]=Intrinsic(new_assign)
                    assigned=True
        if not assigned:
            assign_map[assign]=assign
    routine.body=Transformer(assign_map).visit(routine.body) 
    #TODO ::: MAPING MODIFIER LES A => B en assoc(A,B)
    return(tmp_pt_klon)

def nullify(routine, tmp_pt_klon):
#    null=FindNodes(Nullify).visit(routine.body)
    null_map={}
    for null in FindNodes(Nullify).visit(routine.body):
        new_null=()
        for pt in null.variables:
            if null.variables[0].name in tmp_pt_klon:
                new_intr=Intrinsic("nullptr("+null.variables[0].name+")")
                new_null=new_null+(new_intr,)
        null_map[null]=new_null
    routine.body=Transformer(null_map).visit(routine.body)
#define target lst to avoid A.type => bug



def assoc_alloc_pt(routine):
    temp_map={}
    for decls in FindNodes(VariableDeclaration).visit(routine.spec):
        intrinsic_lst=[]
        var_lst=[]
        for s in decls.symbols:
            if s.name in tmp_pt_klon:
                var=tmp_pt_klon[s.name]
                if var.type.kind:
                    new_s='temp ('+var.type.dtype.name+' (KIND='+var.type.kind.name+'), '+s.name+', ('
                else:
                    new_s='temp ('+var.type.dtype.name+', '+s.name+', ('
                for shape in var.shape:
                    new_s=new_s+str(shape)+', '
                new_s=new_s[:-2]
                new_s=new_s+'))'
                #alloc='alloc ('+s.name+')'
                #routine.spec.append(Intrinsic(alloc))
                intrinsic_lst.append(Intrinsic(new_s))
            else:
                var_lst=[s]
                VAR=decls.clone(symbols=var_lst)
                intrinsic_lst.append(VAR)
        temp_map[decls]=tuple(intrinsic_lst)
    routine.spec=Transformer(temp_map).visit(routine.spec)

pathR=sys.argv[1]
pathW=sys.argv[2]

from pathlib import Path
pathW=pathW.replace(".F90", "")+"_openacc"

#routine=load_subroutine(pathR, fileR, name)
source=Sourcefile.from_file(pathR)
routine=source.subroutines[0]


import logical_lst


horizontal=Dimension(name='horizontal',size='KLON',index='JLON',bounds=['KIDIA','KFDIA'],aliases=['NPROMA','KDIM%KLON','D%INIT'])
horizontal_lst=['JLON', 'JL','JPROF']
vertical=Dimension(name='vertical',size='KLEV',index='JLEV')
#true_symbols=[]
#false_symbols=['LHOOK', 'LMUSCLFA','LFLEXDIA']

horizontal_lst_size=["KLON","YDCPG_OPTS%KLON","YDGEOMETRY%YRDIM%NPROMA","KPROMA", "YDDIM%NPROMA", "NPROMA"]
#horizontal_lst_size=["KLON","YDCPG_OPTS%KLON","YDGEOMETRY%YRDIM%NPROMA","KPROMA"]
horizontal_lst_bounds=[["KIDIA", "YDCPG_BNDS%KIDIA","KST"],["KFDIA", "YDCPG_BNDS%KFIDIA","KEND"]]
 
horizontal_size=get_horizontal(routine, horizontal_lst_size)
resolve_associates(routine)
true_symbols, false_symbols=logical_lst.symbols()
false_symbols.append('LHOOK')
logical.transform_subroutine(routine, true_symbols, false_symbols)

end_index, begin_index, new_range=ExplicitArraySyntaxes.ExplicitArraySyntaxes(routine, horizontal_lst_size, horizontal_lst_bounds)
loop_variable=get_loop_variable(routine, horizontal_lst)
#Scalar("JLON")
#loop_variable=horizontal.index
bounds=[begin_index, end_index]
##add_openacc1(routine)
add_openacc2(routine)

#ResolveVector.resolve_vector_dimension(routine, loop_variable, bounds)
rename(routine)
acc_seq(routine)
stack_mod(routine)
rm_KLON(routine, horizontal, horizontal_size)
#ResolveVector.resolve_vector_dimension(routine, loop_variable, horizontal.bounds)
ResolveVector.resolve_vector_dimension(routine, loop_variable, bounds)

remove_loop(routine)
###
ystack1(routine)
rm_sum(routine)
generate_interface(routine) #must be before temp allocation and y stack, or temp will be in interface

#----------------------------------------
#Pointers
#----------------------------------------
tmp_pt, tmp_target=find_pt(routine)
tmp_pt_klon=get_dim_pt(routine, tmp_pt, horizontal_size)
assoc_alloc_pt(routine)
#----------------------------------------
#
write_print(routine)
nullify(routine, tmp_pt_klon)

ystack2(routine)
alloc_temp(routine)
jlon_kidia(routine, end_index, begin_index, new_range, loop_variable) #at the end


Sourcefile.to_file(fgen(routine), Path(pathW+".F90"))
