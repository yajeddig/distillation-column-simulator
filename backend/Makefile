# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -Wextra -fcheck=all -g
LDFLAGS = 

# Directories
SRC_DIR = src
MOD_DIR = $(SRC_DIR)/modules
THERMO_DIR = $(SRC_DIR)/thermodynamics
SOLVER_DIR = $(SRC_DIR)/solvers
UTILS_DIR = $(SRC_DIR)/utils

# Source files (order matters for Fortran!)
MODULES = $(wildcard $(MOD_DIR)/*.f90)
THERMO = $(wildcard $(THERMO_DIR)/*.f90)
SOLVERS = $(wildcard $(SOLVER_DIR)/*.f90) $(wildcard $(SOLVER_DIR)/*.f)
UTILS = $(wildcard $(UTILS_DIR)/*.f90)
MAIN = $(SRC_DIR)/TESTMAIN.f90

# Object files
MOD_OBJ = $(MODULES:.f90=.o)
THERMO_OBJ = $(THERMO:.f90=.o)
SOLVER_OBJ_F90 = $(filter %.f90, $(SOLVERS))
SOLVER_OBJ_F = $(filter %.f, $(SOLVERS))
SOLVER_OBJ = $(SOLVER_OBJ_F90:.f90=.o) $(SOLVER_OBJ_F:.f=.o)
UTILS_OBJ = $(UTILS:.f90=.o)
MAIN_OBJ = $(MAIN:.f90=.o)

ALL_OBJ = $(MOD_OBJ) $(UTILS_OBJ) $(THERMO_OBJ) $(SOLVER_OBJ) $(MAIN_OBJ)

# Executable
TARGET = distillation

# Default target
all: $(TARGET)

# Link executable
$(TARGET): $(ALL_OBJ)
	$(FC) $(LDFLAGS) -o $@ $^
	@echo "✓ Build successful: $(TARGET)"

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Compile .f (Fortran 77) to .o
%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

# Dependencies (modules must be compiled first)
$(UTILS_OBJ): $(MOD_OBJ)
$(THERMO_OBJ): $(MOD_OBJ)
$(SOLVER_OBJ): $(MOD_OBJ)
$(MAIN_OBJ): $(MOD_OBJ) $(UTILS_OBJ) $(THERMO_OBJ) $(SOLVER_OBJ)

# Clean build artifacts
clean:
	rm -f $(ALL_OBJ) $(TARGET)
	rm -f *.mod *.o
	rm -f $(MOD_DIR)/*.mod $(THERMO_DIR)/*.mod $(SOLVER_DIR)/*.mod $(UTILS_DIR)/*.mod
	@echo "✓ Clean complete"

# Clean and rebuild
rebuild: clean all

# Run simulation
run: $(TARGET)
	./$(TARGET)

# Help
help:
	@echo "Distillation Column Simulator - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all     - Build the executable (default)"
	@echo "  clean   - Remove build artifacts"
	@echo "  rebuild - Clean and build"
	@echo "  run     - Build and run simulation"
	@echo "  help    - Show this message"

.PHONY: all clean rebuild run help
