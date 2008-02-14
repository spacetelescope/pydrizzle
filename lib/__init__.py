from pytools import numerixenv
numerixenv.check()
import string
import pydrizzle
from pytools import fileutil, wcsutil
import pyfits

yes = True  # 1
no = False  # 0
from drutil import DEFAULT_IDCDIR
from math import *

# Version string is in pydrizzle.py
__version__ = pydrizzle.__version__

def PyDrizzle(input, output=None, field=None, units=None, section=None,
        kernel=None,pixfrac=None,bits_final=0,bits_single=0,
        wt_scl='exptime', fillval=0.,idckey='', in_units='counts',
        idcdir=DEFAULT_IDCDIR,memmap=0,dqsuffix=None,prodonly=False,
        shiftfile=None,updatewcs=True):

    import process_input
    asndict, ivmlist, output = process_input.process_input(input, prodonly=prodonly, shiftfile=shiftfile)
    name = fileutil.buildNewRootname(asndict['output'], extn='_asn.fits')
    p = pydrizzle._PyDrizzle(asndict, output=output,field=field,
                             units=units, idckey=idckey, 
                             section=section, kernel= kernel,
                             pixfrac=pixfrac,
                             bits_single=bits_single,
                             bits_final=bits_final,
                             wt_scl=wt_scl, fillval=fillval,
                             in_units=in_units,
                             idcdir=idcdir, memmap=memmap,
                             dqsuffix=dqsuffix)

    
    return p

def help():
    print pydrizzler._PyDrizzle.__doc__
