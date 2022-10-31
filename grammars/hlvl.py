from functools import reduce
from textx.metamodel import metamodel_from_file
import time
import typing
from abc import ABC,  abstractmethod


def parse_hlvl(constraints_strs, solver):
    print(constraints_strs)
    constraints_str = "\n".join(constraints_strs)
    t1 = time.time()
    hlvl_meta = metamodel_from_file(
        './grammars/hlvl.tx',
        classes=[Model, Choice, Relation, Common, Mutex, Implies, Group, Decomposition])
    t2 = time.time()
    model: Model = hlvl_meta.model_from_str(constraints_str)
    # model = hlvl_meta.model_from_file('e_shop2.hlvl', debug=False)
    t3 = time.time()
    print(vars(model))
    print('T Model', t2-t1)
    print('T Parse', t3-t2)
    print('T Total', t3-t1)
    return model.mzn_model() if solver == "minizinc" else model.swi_model()


class Model:
    def __init__(self, name, options, relations):
        self.name = name
        self.options = options
        self.relations = relations

    def mzn_model(self):
        return [o.mzn_model() for o in self.options] + [r.mzn_model() for r in self.relations]

    def swi_model(self):
        return [o.swi_model() for o in self.options] + [r.swi_model() for r in self.relations]


class Option:
    def __init__(self, name, parent, comment):
        self.name = name
        self.parent = parent
        self.comment = comment

    def quoted_name(self):
        return f"'{self.name}'"


class Choice(Option):
    def __init__(self, name, multiplicity, comment, parent):
        super().__init__(name, parent, comment)
        self.multiplicity = multiplicity

    def mzn_model(self):
        return f"var {self.multiplicity.lower}..{self.multiplicity.upper}:{self.quoted_name()};"

    def swi_model(self):
        return f"{self.name} in {self.multiplicity.lower}..{self.multiplicity.upper}"

# class Enum:
#     def __init__(self, name, multiplicity, domain, comment, parent):
#         self.name = name
#         self.multiplicity = multiplicity
#         self.domain = domain
#         self.comment = comment
#         self.parent = parent

#     def mzn_model(self):
#         return f"var {self.multiplicity.lower}..{self.multiplicity.upper}:{self.name}"


def c_names(options, quoted: bool):
    return [(o.quoted_name() if quoted else o.name) for o in options]


class Relation:
    def __init__(self, name, relation, parent):
        self.name = name
        self.relation = relation
        self.parent = parent

    def mzn_model(self):
        return "constraint " + self.relation.mzn_model() + ";"

    def swi_model(self):
        return self.relation.swi_model()


class Common:
    def __init__(self, choices, parent):
        self.choices = choices
        self.parent = parent

    def mzn_model(self):
        return f"{' + '.join(c_names(self.choices, True))} == {len(self.choices)}"

    def swi_model(self):
        return f"{' + '.join(c_names(self.choices, False))} #= {len(self.choices)}"


class Mutex:
    def __init__(self, choices, parent):
        self.choices = choices
        self.parent = parent

    def mzn_model(self):
        return f"{' + '.join(c_names(self.choices, True))} <= 1"

    def swi_model(self):
        return f"{' + '.join(c_names(self.choices, False))} #=< 1"


class Implies:
    def __init__(self, condition, consequence, parent):
        self.condition = condition
        self.consequence = consequence
        self.parent = parent

    def mzn_model(self):
        return f"({self.condition.quoted_name()} == 1) -> ({self.consequence.quoted_name()} == 1)"
    
    def swi_model(self):
        return f"({self.condition.name} #= 1) #==> ({self.consequence.name} #= 1)"


class Group:
    def __init__(self, prt, choices, multiplicity, parent):
        self.prt = prt
        self.choices = choices
        self.parent = parent
        self.multiplicity = multiplicity

    def mzn_model(self):
        sum_str = " + ".join(c_names(self.choices, True))
        return f"({self.prt.quoted_name()} * {self.multiplicity.lower} <= {sum_str}) /\\ ({sum_str} <= {self.prt.quoted_name()} * {self.multiplicity.upper})"

    def swi_model(self):
        sum_str = " + ".join(c_names(self.choices, False))
        return f"({self.prt.name} * {self.multiplicity.lower} #=< {sum_str}) #/\\ ({sum_str} #=< {self.prt.name} * {self.multiplicity.upper})"


class Decomposition:
    def __init__(self, prt, choices, multiplicity, parent):
        self.prt = prt
        self.choices = choices
        self.parent = parent
        self.multiplicity = multiplicity

    def mzn_model(self):
        sum_str = " + ".join(c_names(self.choices, True))
        return f"({self.prt.quoted_name()} * {self.multiplicity.lower} <= {sum_str}) /\\ ({sum_str} <= {self.prt.quoted_name()} * {self.multiplicity.upper})"

    def swi_model(self):
        sum_str = " + ".join(c_names(self.choices, False))
        return f"({self.prt.name} * {self.multiplicity.lower} #=< {sum_str}) #/\\ ({sum_str} #=< {self.prt.name} * {self.multiplicity.upper})"
