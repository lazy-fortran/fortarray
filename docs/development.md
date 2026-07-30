---
title: Development
---

# Development

Generated HTML, compiler modules, binaries, test data, and benchmark output
must stay outside the source tree or in ignored build directories.

Before submitting a change:

```sh
fo
cmake -S . -B build-cmake \
  -DFORTARRAY_BUILD_TESTS=ON \
  -DFORTARRAY_BUILD_BENCHMARKS=ON
cmake --build build-cmake --parallel
ctest --test-dir build-cmake --output-on-failure
ford --output_dir /tmp/fortarray-site ford.md
python3 test/check_docs.py /tmp/fortarray-site
git status --short
```

Tests need an independent behavioral oracle. Repository-state assertions that
only restate the implementation are not tests.

Pull requests build and validate Pages without deployment. Pushes to `main`
deploy the generated artifact; generated site files are never committed.
