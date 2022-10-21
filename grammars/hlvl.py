#!/usr/bin/env python3

from textx.metamodel import metamodel_from_file
import time


def parse_hlvl(constraints_strs):
    print(constraints_strs)
    constraints_str = "\n".join(constraints_strs)
    t1 = time.time()
    hlvl_meta = metamodel_from_file('./grammars/hlvl.tx')
    t2 = time.time()
    model = hlvl_meta.model_from_str(constraints_str)
    # model = hlvl_meta.model_from_file('e_shop2.hlvl', debug=False)
    t3 = time.time()
    print(vars(model))
    print('T Model', t2-t1)
    print('T Parse', t3-t2)
    print('T Total', t3-t1)
    return model
