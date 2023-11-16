from loki import (Frontend, Sourcefile, Scheduler, FindNodes, Loop, Variable,
            Assignment, CallStatement, Transformation, Node, SymbolAttributes, 
            DerivedType, BasicType, Import, Transformer, Conditional, SchedulerConfig
            )
from loki.expression import LoopRange, FindVariables, FindTypedSymbols
from loki.expression.expr_visitors import SubstituteExpressions
from loki.expression.symbols import (
    Array, Scalar, InlineCall, TypedSymbol, FloatLiteral, IntLiteral, LogicLiteral,
    StringLiteral, IntrinsicLiteral, DeferredTypeSymbol, LogicalOr, LogicalAnd, LogicalNot, RangeIndex
)
from loki.ir import Section, Comment, VariableDeclaration, Import, Intrinsic
from pathlib import Path
from termcolor import colored
from loki.logging import info
import sys

def ExplicitArraySyntaxes(routine, lst_horizontal_size, lst_horizontal_bounds):
    
  total = 0
  assign_map={}
  
  begin_index = None
  end_index = None

  verbose=False
#  verbose=True

  define=False
  splitted1 = lst_horizontal_bounds[0][1].split('%')
  splitted2 = lst_horizontal_bounds[1][1].split('%')
  for var in FindVariables().visit(routine.variables):

    if (var.name == splitted1[0]):
      begin_index=Variable(name=f'{var.name}%{splitted1[1]}', parent=var, scope=routine)
      if verbose : print(colored("derived type " + splitted1[0] + " found", "green"))


    if (var.name == lst_horizontal_bounds[0][0]):
      begin_index = var
      if verbose : print(colored("variable " + var.name + " found", "green"))

    if (var.name == lst_horizontal_bounds[0][2]):
      begin_index = var
      if verbose : print(colored("variable " + var.name + " found", "green"))

    if (var.name == splitted2[0]):
      end_index=Variable(name=f'{var.name}%{splitted2[1]}', parent=var, scope=routine)
      if verbose : print(colored("derived type " + splitted2[0] + " found", "green"))

    if (var.name == lst_horizontal_bounds[1][0]):
      end_index = var
      if verbose : print(colored("variable " + var.name + " found", "green"))

    if (var.name == lst_horizontal_bounds[1][2]):
      end_index = var
      if verbose : print(colored("variable " + var.name + " found", "green"))

  if verbose: print("begin_index=",begin_index)
  if verbose: print("end_index=",end_index)

  for assign in FindNodes(Assignment).visit(routine.body):
    is_pointer=False
    not_found=[]  
    expression_map={}             
    for var in FindVariables().visit(assign):
      if (var.type.pointer):
        is_pointer=True
        if verbose : print("Variable", var.name, " is a pointer")
      if isinstance(var, Array):
        if (len(var.dimensions) > 0):
          if (var.dimensions[0] == ':'):              

            if not (begin_index or end_index):
                raise RuntimeError(f'index variables not found in routine {routine.name}')
           # Check if variable seems to be of the correct size
            found_in_routine_vars = False
            if (var.name in routine.variable_map) :
              if verbose : print ("Variable", var.name, " found in routine variables")
              routine_var =routine.variable_map[var.name]
              dim_name = routine_var.dimensions[0].name
              if (dim_name in lst_horizontal_size):
                if verbose : print(colored("First dimension of array is "+dim_name, "green"))
                newrange = RangeIndex( (begin_index, end_index, None) )
                define=True
                newdimensions = (newrange,) + var.dimensions[1:]
                new_var = var.clone(dimensions=newdimensions)
                expression_map[var] = new_var
              else:
                print(colored("Unexpected first dimension of array : " + dim_name, "red"))

#            elif (var.type.pointer) :
#              is_pointer=True
#              if verbose : print("Variable", var.name, " is a pointer")
              
            else :
              not_found.append(var.name)

    if not is_pointer : 
      for var in not_found :
        print (colored("Variable not found in routine variables !", "red"))
        if verbose : print ("Variable", var, " not found in routine variables !")

    if expression_map:
      total += len(expression_map)
      explicited_assign = SubstituteExpressions(expression_map).visit(assign)
      assign_map[assign] = explicited_assign
      if verbose : print("Transforming ", assign)
      if verbose : print("        into ", explicited_assign,  "\n")
          
  routine.body=Transformer(assign_map).visit(routine.body)
  if verbose:
      if (total > 0) : info(f'[Loki] {routine.name}:: {total} implicit array syntax expressions replaced with explicit boundaries')
  if define==False :
    newrange=None
  return(end_index, begin_index, newrange)
