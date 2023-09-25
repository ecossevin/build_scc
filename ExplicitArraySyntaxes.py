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

def ExplicitArraySyntaxes(routine, horizontal, horizontal_lst):
    
  horizontal_lst_size=["KLON","YDCPG_OPTS%KLON"]
  horizontal_lst_bounds=[["KIDIA", "YDCPG_BNDS%KIDIA"],["KFDIA", "YDCPG_BNDS%KFIDIA"]]
  total = 0
  assign_map={}
  
  begin_index = None
  end_index = None

  verbose=True

  splitted = horizontal_lst_bounds[0][1].split('%')
  for var in FindVariables().visit(routine.variables):
    if (var.name == splitted[0]):
      begin_index=Variable(name=f'{var.name}%{splitted[1]}', parent=var, scope=routine)
      if verbose : print(colored("derived type " + splitted[0] + " found", "green"))


  for var in FindVariables().visit(routine.variables):
    if (var.name == horizontal_lst_bounds[0][0]):
      begin_index = var
      if verbose : print(colored("variable " + var.name + " found", "green"))

  splitted = horizontal_lst_bounds[1][1].split('%')
  for var in FindVariables().visit(routine.variables):
    if (var.name == splitted[0]):
      end_index=Variable(name=f'{var.name}%{splitted[1]}', parent=var, scope=routine)
      if verbose : print(colored("derived type " + splitted[0] + " found", "green"))

  for var in FindVariables().visit(routine.variables):
    print(var.name)
    if (var.name == horizontal_lst_bounds[1][0]):
      end_index = var
      if verbose : print(colored("variable " + var.name + " found", "green"))



  for assign in FindNodes(Assignment).visit(routine.body):
    expression_map={}             
    for var in FindVariables().visit(assign):
      if isinstance(var, Array):
        if (len(var.dimensions) > 0):
          if (var.dimensions[0] == ':'):              

#            if not (begin_index or end_index):
#             raise RuntimeError(f'index variables not found in routine {routine.name}')
           # Check if variable seems to be of the correct size
            found_in_routine_vars = False
            if (var.name in routine.variable_map) :
              if verbose : print ("Variable", var.name, " found in routine variables")
              routine_var =routine.variable_map[var.name]
              dim_name = routine_var.dimensions[0].name
              if (dim_name in horizontal_lst_size):
                if verbose : print(colored("First dimension of array is "+dim_name, "green"))
                newrange = RangeIndex( (begin_index, end_index, None) )
#                newrange = RangeIndex( (horizontal_index, end_index, None) )
#                if verbose: 
#                  print(colored("begin_index=",begin_index))
#                  print(colored("end_index=",end_index))
#                  print(colored("newrange=", newrange))
#
                newdimensions = (newrange,) + var.dimensions[1:]
                new_var = var.clone(dimensions=newdimensions)
                expression_map[var] = new_var
              else:
                print(colored("Unexpected first dimension of array : " + dim_name, "red"))

            else :
              print (colored("Variable not found in routine variables !", "red"))

    if expression_map:
      total += len(expression_map)
      explicited_assign = SubstituteExpressions(expression_map).visit(assign)
      assign_map[assign] = explicited_assign
      if verbose : print("Transforming ", assign)
      if verbose : print("        into ", explicited_assign,  "\n")
          
  routine.body=Transformer(assign_map).visit(routine.body)
  if (total > 0) : info(f'[Loki] {routine.name}:: {total} implicit array syntax expressions replaced with explicit boundaries')

  return(end_index, begin_index, newrange)
