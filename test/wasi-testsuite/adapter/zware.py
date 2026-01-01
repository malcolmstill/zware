"""
WASI Testsuite adapter for zware

This adapter enables the WASI testsuite to test the zware WebAssembly runtime.
The adapter translates WASI test requirements into zware-run command-line arguments.
"""

import os
import subprocess
import shlex
from pathlib import Path
from typing import Dict, List, Tuple

# Path to the zware-run executable, can be overridden via environment variable
ZWARE_RUN = shlex.split(os.environ.get("ZWARE_RUN", "zig-out/bin/zware-run"))


def get_name() -> str:
    """Return the name of this adapter"""
    return "zware"


def get_version() -> str:
    """Return the version of zware"""
    try:
        result = subprocess.run(
            ZWARE_RUN[0:1] + ["--version"],
            encoding="UTF-8",
            capture_output=True,
            check=True
        )
        # Parse version from output (format: "zware-run X.Y.Z")
        # Note: --version prints to stderr
        output = result.stderr if result.stderr else result.stdout
        if output:
            parts = output.strip().split()
            if len(parts) >= 2:
                return parts[-1]  # Return last word as version
        return "unknown"
    except (subprocess.SubprocessError, FileNotFoundError):
        return "unknown"


def get_wasi_versions() -> List[str]:
    """Return list of supported WASI versions"""
    # zware currently supports WASI preview 1 (wasi_snapshot_preview1)
    return ["wasm32-wasip1"]


def compute_argv(test_path: str,
                 args: List[str],
                 env: Dict[str, str],
                 dirs: List[Tuple[Path, str]],
                 wasi_version: str) -> List[str]:
    """
    Compute the command-line arguments for running a WASI test

    Args:
        test_path: Path to the .wasm file to execute
        args: Command-line arguments to pass to the WASM module
        env: Environment variables to set
        dirs: Directory mappings for preopens (list of (host_path, guest_path) tuples)
        wasi_version: WASI version to use (currently only "wasm32-wasip1" is supported)

    Returns:
        List of command-line arguments for executing the test with zware-run
    """
    argv = [] + ZWARE_RUN

    # Isolate stdio for tests to prevent blocking on stdin
    # Tests should not interact with the host's terminal
    argv.append("--no-inherit-stdio")

    # Add environment variables using --env KEY=VALUE
    for key, value in env.items():
        argv.extend(["--env", f"{key}={value}"])

    # Add directory mappings (preopens) using --dir GUEST::HOST
    for host_path, guest_path in dirs:
        argv.extend(["--dir", f"{guest_path}::{host_path}"])

    # Add the WASM module path
    # The testsuite expects to call _start (WASI entry point), which is zware-run's default
    argv.append(test_path)

    # Add arguments to pass to the WASM module
    argv.extend(args)

    return argv
