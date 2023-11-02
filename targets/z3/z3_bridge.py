from dataclasses import dataclass
import time
import z3

from targets.z3.z3_model import Z3Model
from variamos import query


@dataclass
class Z3Bridge:

    def translate(self, model: Z3Model):
        raise NotImplementedError("Z3 does not generate a program to return")

    def solve(self, model: Z3Model, n_sols: int = 1):
        s = z3.Solver()
        s.add(model.constraints)
        return run_iterative_solve(s, model, n_sols)

    def optimize(
        self,
        model: Z3Model,
        objective: str,
        direction: query.OptimizationDirectionEnum,
        n_sols=1,
    ):
        if objective not in model.var_decls:
            raise Exception("Objective not in model")
        obj_var = model.var_decls[objective]
        o = z3.Optimize()
        o.add(model.constraints)
        if direction == query.OptimizationDirectionEnum.min:
            o.minimize(obj_var)
        else:
            o.maximize(obj_var)
        return run_iterative_solve(o, model, n_sols)


def run_iterative_solve(s: z3.Solver | z3.Optimize, model: Z3Model, n_sols):
    time_limit = 60.0
    sols: list[dict] = []
    start_time = time.perf_counter()
    print("starting timer ", start_time)
    all_smt_gen = all_smt(s, model.var_decls.values())
    while (
        not ((loop_time := time.perf_counter()) - start_time > time_limit)
        and len(sols) < n_sols
    ):
        try:
            result = next(all_smt_gen)
            sols.append(
                {
                    var_name: result[var]
                    for var_name, var in model.var_decls.items()
                }
            )
            print("loop time ", loop_time)
        except StopIteration:
            break
    return sols


def all_smt(s: z3.Solver | z3.Optimize, initial_terms):
    def block_term(s, m, t):
        s.add(t != m.eval(t, model_completion=True))

    def fix_term(s, m, t):
        s.add(t == m.eval(t, model_completion=True))

    def all_smt_rec(terms):
        if z3.sat == s.check():
            m = s.model()
            yield m
            for i in range(len(terms)):
                s.push()
                block_term(s, m, terms[i])
                for j in range(i):
                    fix_term(s, m, terms[j])
                yield from all_smt_rec(terms[i:])
                s.pop()
        else:
            return

    yield from all_smt_rec(list(initial_terms))
