"""
File: 
    generic_COBALT_diagyaml2register_diag_body.py
Author: 
    Kelly Kearney
Date: 
    2024/03/27
Description: 
    This script builds the Fortran source code that makes up the primary body of the 
    generic_COBALT_register_diag subroutine.  This routine allows for easy maintenance of the 
    highly-repetitive and lengthy code.  The resulting generic_COBALT_register_diag_body.f90 is 
    added to the primary generic_COBALT.f90 module via an include statement, and can be quickly 
    regenerated when new diagnostics are added or the specific commands in question need to be 
    modified.
Usage:
    python generic_COBALT_diagyaml2register_diag_body.py formatopt

    formatopt is a string indicating the code format to use:
        original:   The format in the original generic_COBALT.  A (temporary) vardesc-formatted 
                    variable is created for each diag, then register_diag_field is called using 
                    fields from that, using pass-by-position syntax for the first 6 inputs and 
                    name/value for any additional (default if script called w/o input)
        novardesc:  This format eliminates the creation of an intermediate vardesc variable 
"""

import yaml
import sys

# Parse and check input

if len(sys.argv) < 1:
    formatopt = "original"
else:
    formatopt = sys.argv[1]

if not formatopt in ["original", "novardesc"]:
    raise Exception("Unrecognized format option")

# Read diagnostics from yaml file

yamlfile = 'generic_COBALT_diag.yaml'
with open(yamlfile, 'r') as f:
    diag = yaml.safe_load(f)
    
    
# List of primary parameter names.  These include the vardesc type fields and the inputs to
# register_diag_field that were passed by position (rather than name/value) in the original
# generic_COBALT code.  

primary = ["name", "longname", "hor_grid", "z_grid", "t_grid", "units", "mem_size", 
               "var_to_register", "axes"]

# Open file for writing

f = open("generic_COBALT_register_diag_body.f90", "w")

# Reformat each diagnostic into the vardesc and register_diag_field calls

for d in diag:

    # Does this diag variable include extra fields to be passed to the register_diag_field function
    # via name=value syntax?  If so, build that part of the code here
    
    nvstr = ""
    for key, value in d.items():
        if not key in primary:
            nvstr = ", &\n          ".join([nvstr, "{} = {}".format(key,value)])
    
    # print(nvstr)

    if (formatopt == "original"):
        
        # The format in the original generic_COBALT.  A (temporary) vardesc-formatted variable is
        # created for each diag, then register_diag_field is called using fields from that, using
        # pass-by-position syntax for the first 6 inputs and name/value for any additional
    
        mystr = ("     vardesc_temp = vardesc({name}, {longname}, {hor_grid}, {z_grid}, {t_grid}, {units}, {mem_size})\n"
                 "     {var_to_register} = register_diag_field(package_name, vardesc_temp%name, {axes}, &\n"
                 "          init_time, vardesc_temp%longname, vardesc_temp%units"
                ).format(**d)
        mystr = mystr + nvstr + ")\n"
        
    elif (formatopt == "novardesc"):
        
        # This format eliminates the creation of an intermediate vardesc variable 
    
        mystr = ( "     {var_to_register} = register_diag_field(package_name, {name} {axes}, &\n"
                 "          init_time, {longname}, {units}"
                ).format(**d)
        mystr = mystr + nvstr + ")\n"
        
        
    # Write to file

    if d['name'] == '"b_di14c"': # begin radiocarbon block
        f.write("     if (do_14c) then\n")
    
    f.write(mystr)
    
    if d['name'] == '"jdo14c"': # end radiocarbon block
        f.write("     endif\n")
    
# Close file

f.close()




    
