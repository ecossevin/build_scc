from loki import Sourcefile , Dimension, fgen

from pathlib import Path

import os
import sys
import copy

import re
import click

from  scctransfo import openacc_trans

#*********************************************************
#      Functions    needed    for    inlining....
#*********************************************************

def add_contains(pathpack, pathview, pathfile, pathacc, horizontal_opt, inlined=False):
#def inline_calls(pathpack, pathview, pathfile, pathacc, horizontal_opt, inlined):
    """
    Add routine to inline as contained subroutine (loki's inliner can only inline contained subroutines)
    :param pathpack: absolute path to the pack
    :param pathview: path to the src/local/... or src/main/... folder
    :param pathfile: path (+name) to the file
    :param pathacc: path (+name) to the acc file 
    :param horizontal_opt: additional horizontal loop index
    :param inlined: lst of routines to inline
    """
    verbose=False
    #verbose=True
    pathr=pathpack+'/'+pathview+pathfile
    match_inline=False
    dict_callee_path={}
    if inlined != False:    
           
  #creation of a dict associating the name of each callee to inline to it's path; according to the path in the openacc.sh file 
  #TODO : create a file containing these path? 
#        with open('openacc.sh', 'r', encoding='utf-8', errors='ignore') as file_lst_callee:
        with open('/home/gmap/mrpm/cossevine/build_scc/openacc.sh', 'r') as file_lst_callee:
            lines=file_lst_callee.readlines()
            for callee_name in inlined:
                callee_path=None
                for line in lines:
                    callee_path=re.search('((\w*\/)+'+callee_name+')', line)
                    if callee_path:
                        
                        callee_path=pathpack+'/'+pathview+callee_path.group(0)
                        dict_callee_path[callee_name]=callee_path
        if verbose: print("dict_callee_path=",dict_callee_path)

   #open the current routine "caller", inserts each callee when a CALL CALLEE appears in the caller. 
        #with open(pathr, 'r') as file_caller:
        with open(pathr, 'r', encoding='utf-8', errors='ignore') as file_caller:
            caller = file_caller.read()
                
        for callee_name in inlined: #look for each callee sub  in the caller
            if caller.find("CALL "+callee_name.replace(".F90","").upper())!=-1:
                if verbose: print("callee_name = ", callee_name)
                #with open(dict_callee_path[callee_name], 'r') as file_callee:
                with open(dict_callee_path[callee_name], 'r', encoding='utf-8', errors='ignore') as file_callee:
                    callee = file_callee.read()
                if not match_inline: #add CONTAINS only for the first callee matching
                    match_inline=True
                    callee="CONTAINS\n\n"+callee
                    loc=caller.find("END SUBROUTINE")
                    if loc != -1 :
                        caller=caller[:loc]+callee+caller[loc:]
                else: #add the callee after the CONTAINS statement
                    loc=caller.find("CONTAINS")
                    if loc != -1 :
                         loc=loc+len("CONTAINS\n\n") #insert after CONTAINS
                         caller=caller[:loc]+callee+caller[loc:]

        if verbose: print(pathpack+"/tmp/"+os.path.basename(pathfile))
        if match_inline:
            with open(pathpack+"/tmp/"+os.path.basename(pathfile), "w", encoding='utf-8', errors='ignore') as file_caller:
           # with open(pathpack+"/tmp/"+os.path.basename(pathfile), "w") as file_caller:
                file_caller.write(caller)
    else:
        if verbose: print(colored("no routine to inline", "red"))


    return(match_inline)

#diff way to do : uniformize loop or re.search(_LOOPIDX) 
def rename_hor(routine, lst_horizontal_idx): 
    """
    When the inlining is done the loop index may be CALLEE_LOOPIDX, this routine changes it to LOOPIDX
    :param routine:.
    :param lst_horizontal_idx: list of aliases of NPROMA, you may add new ones
    """
    rename_map={}
    for var in FindVariables().visit(routine.body):
        for idx in lst_horizontal_idx:
            if '_'+idx in var.name:
                rename_map[var]=var.clone(name=idx)
    routine.body=SubstituteExpressions(rename_map).visit(routine.body)
                    
def mv_include(routine):
    """
    Ecmwf inlining function doesn't seem to move the include from the CALLEE to the CALLER : it's done in this function.
    :param routine:.
    """
    imp_lst=[imp.module for imp in FindNodes(ir.Import).visit(routine.spec)]
    for containedSubroutine in routine.subroutines:
        for imp in FindNodes(ir.Import).visit(containedSubroutine.spec):
            if not imp.module in imp_lst:
                imp_lst.append(imp.module)
                routine.spec.append(imp)
                #routine.spec.insert(0,imp)

#*********************************************************
#         Calling            transformation 
#*********************************************************
   
@click.command()
#@click.option('--pathr', help='path of the file to open')
#@click.option('--pathw', help='path of the file to write to')
@click.option('--pathpack', help='absolute path to the pack')
@click.option('--pathview', help='path to src/local/... or src/main/...')
@click.option('--pathfile', help='path to the file, with the file name in the path')
@click.option('--pathacc', help='path to the place where acc files are stored')

@click.option('--horizontal_opt', default=None, help='additionnal possible horizontal idx')
@click.option('--inlined', '-in', default=None, multiple=True, help='names of the routine to inline')



def call_openacc_trans(pathpack, pathview, pathfile, pathacc, horizontal_opt, inlined):
    """
    Call the openacc_trans. This function is designed to be called from the openacc.sh script. The function is called for each file and processes one file at a time.
    Setup is done here : 
    """
#*********************************************************
#                Setup  
#*********************************************************

    verbose=False
#    verbose=True

    pathr=pathpack+'/'+pathview+pathfile
    pathw=pathpack+pathacc+'/'+pathfile

    pathw=pathw.replace(".F90", "")+"_openacc"

    if verbose: print('pathr=', pathr)
    if verbose: print('pathw=', pathw)
    #----------------------------------------------
    #inlining : 
    #----------------------------------------------
    inlined=list(inlined)
    if inlined:
        if verbose: print("****************INLINING****************")
        inline_match=add_contains(pathpack, pathview, pathfile, pathacc, horizontal_opt, inlined)
    else:
        inline_match=False

    if inline_match:
        filename=os.path.basename(pathfile)
        source=Sourcefile.from_file(pathpack+"/tmp/"+filename)
    else:
        source=Sourcefile.from_file(pathr)
    routine=source.subroutines[0]

    import logical_lst
    
    horizontal=Dimension(name='horizontal',size='KLON',index='JLON',bounds=['KIDIA','KFDIA'],aliases=['NPROMA','KDIM%KLON','D%INIT'])
    vertical=Dimension(name='vertical',size='KLEV',index='JLEV')
    
     #lst_horizontal_idx=['JLON','JROF','JL']
    lst_horizontal_idx=['JLON','JROF']
    #the JL idx have to be added only when it's used at an horizontal idx, because it's used as avertical idx in some places.... this should be fixed... The transformation script could check wether JL is a hor or vert idx instead of adding JL to the lst_horizontal_idx conditionally. 
    if horizontal_opt is not None:
        lst_horizontal_idx.append(horizontal_opt)
    if verbose: print("lst_horizontal_idx=",lst_horizontal_idx)
    
    lst_horizontal_size=["KLON","YDCPG_OPTS%KLON","YDGEOMETRY%YRDIM%NPROMA","KPROMA", "YDDIM%NPROMA", "NPROMA"]
    lst_horizontal_bounds=[["KIDIA", "YDCPG_BNDS%KIDIA","KST"],["KFDIA", "YDCPG_BNDS%KFDIA","KEND"]]
    
    true_symbols, false_symbols=logical_lst.symbols()
    false_symbols.append('LHOOK')
    
#*********************************************************
#       Calling  the       transformation
#*********************************************************


    str_interface = openacc_trans(routine, horizontal, lst_horizontal_idx, lst_horizontal_size, lst_horizontal_bounds, true_symbols, false_symbols, inline_match)

#*********************************************************
#                 Saving   to    file
#*********************************************************

    #interface
    Sourcefile.to_file(str_interface, Path(pathw+".intfb.h"))

    #routine
    Sourcefile.to_file(fgen(routine), Path(pathw+".F90"))
    if inline_match:
        print("inline_match = ", inline_match)
        with open(pathw+".F90", 'r', encoding='utf-8', errors='ignore') as file_caller:
            caller = file_caller.read()
            new_caller=caller.replace("CONTAINS", "")

        with open(pathw+".F90", 'w', encoding='utf-8', errors='ignore') as file_caller:
            file_caller.write(new_caller)
#            file_caller.write(caller)


call_openacc_trans()
