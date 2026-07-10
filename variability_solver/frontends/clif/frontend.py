from variability_solver.frontends.clif.lowerer import lower_clif
from variability_solver.frontends.clif.parser import parse_clif
from variability_solver.frontends.frontend import Frontend
from variability_solver.ir.model import Model


class CLIFFrontend(Frontend):

    @property
    def name(self) -> str:
        return "CLIF"

    @property
    def description(self) -> str:
        return "Common Logic Interchange Format as defined in the ISO/IEC 24707 standard for Common Logic"

    @property
    def file_extensions(self) -> tuple[str, ...]:
        return (".clif",)

    def parse(self, input_str: str) -> Model:
        return lower_clif(parse_clif(input_str))
