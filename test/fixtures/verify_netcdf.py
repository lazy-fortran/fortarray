#!/usr/bin/env python3
import subprocess
import sys

dump = subprocess.run(
    ["ncdump", "-v", "radius,potential", sys.argv[1]],
    check=True,
    capture_output=True,
    text=True,
).stdout
normalized = " ".join(dump.split())

required = [
    "radius = 2",
    "theta = 3",
    "potential:units = \"V\"",
    "radius = 0.25, 0.75",
    "potential =",
    "11, 12, 21, 22, 31, 32",
]
missing = [fragment for fragment in required if fragment not in normalized]
if missing:
    raise SystemExit(f"ncdump oracle mismatch; missing {missing}\n{dump}")
