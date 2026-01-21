# Fortran Distillation Column Simulator - Backend

## Build Instructions

### Prerequisites
- gfortran compiler (GCC Fortran)
- Make

### Quick Start

```bash
# Build the executable
make

# Run a simulation (uses config files in ../config/)
./distillation

# Clean build artifacts
make clean
```

### Compilation Details

The Makefile compiles sources in the correct order:
1. Modules (MODULE_*.f90)
2. Utility subroutines
3. Thermodynamic models
4. Linear solvers
5. Main program

Output: `distillation` executable

### Code Organization

```
src/
├── modules/          # Fortran modules (global variables)
├── thermodynamics/   # NRTL model, equilibrium, enthalpies
├── solvers/          # MRSL01, MRSL21 linear solvers
├── utils/            # Utility functions (norms, residuals)
└── TESTMAIN.f90      # Main program
```

## Original Code (2018)

This backend preserves the original Fortran 90 implementation from the ENSGTI project with added:
- Detailed comments
- Modern build system
- Structured organization

For the original unmodified code, see branch `archive/ensgti-2018`.
