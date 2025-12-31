#!/usr/bin/env python3
"""
Discover all WASI tests from the testsuite and output as JSON.
This is used by both build.zig and GitHub Actions to programmatically
create individual test steps.
"""

import json
import sys
from pathlib import Path
from typing import List, Dict


def discover_tests(testsuite_root: Path) -> Dict[str, List[str]]:
    """
    Discover all WASI tests in the testsuite.

    Returns:
        Dict mapping suite name to list of test names
    """
    suites = {
        "c": "tests/c/testsuite/wasm32-wasip1",
        "rust": "tests/rust/testsuite/wasm32-wasip1",
        "as": "tests/assemblyscript/testsuite/wasm32-wasip1",
    }

    results = {}

    for suite_name, suite_path in suites.items():
        full_path = testsuite_root / suite_path
        if not full_path.exists():
            continue

        test_names = []
        for wasm_file in sorted(full_path.glob("*.wasm")):
            test_name = wasm_file.stem  # filename without extension
            test_names.append(test_name)

        results[suite_name] = test_names

    return results


def main():
    if len(sys.argv) < 2:
        print("Usage: discover-wasi-tests.py TESTSUITE_ROOT [--format FORMAT]", file=sys.stderr)
        print("Formats: json (default), zig-array, github-matrix", file=sys.stderr)
        return 1

    testsuite_root = Path(sys.argv[1])
    output_format = "json"

    if len(sys.argv) >= 4 and sys.argv[2] == "--format":
        output_format = sys.argv[3]

    tests = discover_tests(testsuite_root)

    if output_format == "json":
        print(json.dumps(tests, indent=2))

    elif output_format == "zig-array":
        # Output format suitable for embedding in Zig code
        for suite_name, test_names in tests.items():
            print(f'// {suite_name} tests ({len(test_names)})')
            print(f'const wasi_{suite_name}_tests = [_][]const u8{{')
            for test_name in test_names:
                print(f'    "{test_name}",')
            print('};')
            print()

    elif output_format == "github-matrix":
        # Output format for GitHub Actions matrix
        include = []
        for suite_name, test_names in tests.items():
            for test_name in test_names:
                include.append({
                    "suite": suite_name,
                    "test": test_name
                })
        print(json.dumps({"include": include}))

    else:
        print(f"Unknown format: {output_format}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
