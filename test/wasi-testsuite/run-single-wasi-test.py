#!/usr/bin/env python3
"""
Wrapper script to run a single WASI test from the testsuite.
The official test runner only accepts directories, so this script creates
a temporary directory with the single test and runs it.
"""

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description="Run a single WASI test")
    parser.add_argument("test_suite_dir", help="Path to test suite directory")
    parser.add_argument("test_name", help="Name of the test (without .wasm extension)")
    parser.add_argument("--runtime-adapter", "-r", required=True, help="Path to runtime adapter")
    parser.add_argument("--test-runner", required=True, help="Path to wasi_test_runner.py")

    args = parser.parse_args()

    test_suite_dir = Path(args.test_suite_dir)
    test_name = args.test_name

    # Check if test files exist
    wasm_file = test_suite_dir / f"{test_name}.wasm"
    if not wasm_file.exists():
        print(f"Error: Test file {wasm_file} not found", file=sys.stderr)
        return 1

    # Create temporary directory with just this test
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)

        # Copy manifest.json if it exists
        manifest_file = test_suite_dir / "manifest.json"
        if manifest_file.exists():
            shutil.copy(manifest_file, temp_path / "manifest.json")

        # Copy test files
        shutil.copy(wasm_file, temp_path / f"{test_name}.wasm")

        json_file = test_suite_dir / f"{test_name}.json"
        if json_file.exists():
            shutil.copy(json_file, temp_path / f"{test_name}.json")

        # Copy any directories from the test suite (like fs-tests.dir)
        # These are needed for tests that access the filesystem
        for item in test_suite_dir.iterdir():
            if item.is_dir():
                shutil.copytree(item, temp_path / item.name)

        # Run the test runner
        cmd = [
            "python3",
            args.test_runner,
            "-t", str(temp_path),
            "-r", args.runtime_adapter,
        ]

        result = subprocess.run(cmd)
        return result.returncode


if __name__ == "__main__":
    sys.exit(main())
