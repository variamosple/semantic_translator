from dataclasses import dataclass
from swiplserver import (
    PrologMQI,
    PrologThread,
    PrologResultNotAvailableError,
    create_posix_path,
)
import tempfile


@dataclass
class UvlCLIFGenerator:
    model_str: str

    def generate_logic_model(self):
        path = create_posix_path("generators/uvl_generator/ast.pl")
        with tempfile.NamedTemporaryFile(suffix=".uvl", delete=True) as f:
            f.write(self.model_str.encode())
            f.flush()
            f.seek(0)
            uvl_path = create_posix_path(f.name)
            with PrologMQI() as mqi:
                with PrologThread(mqi) as prolog_thread:
                    prolog_thread.query(f"consult('{path}')")
                    return prolog_thread.query(
                        f"print_clif_from_ast('{uvl_path}', CLIFS)", 100
                    )[0]["CLIFS"]
