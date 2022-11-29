from utils.enums import TargetLang
from targets.minzinc.minizinc_bridge import MiniZincBridge


class SolverController:
    def __init__(self, target_lang) -> None:
        self.target_lang = target_lang
        if target_lang == TargetLang.minizinc:
            self.bridge = MiniZincBridge()
        elif target_lang == TargetLang.swi:
            # FIXME: Must implement this
            raise NotImplementedError("SWI controller not done")
        else:
            raise NotImplementedError("Unsupported target backend")
