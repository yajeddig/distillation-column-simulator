# 🧪 Distillation Column Simulator

[![Fortran](https://img.shields.io/badge/Fortran-90-734F96?logo=fortran&logoColor=white)](https://fortran-lang.org/)
[![Python](https://img.shields.io/badge/Python-3.8+-3776AB?logo=python&logoColor=white)](https://python.org/)
[![Streamlit](https://img.shields.io/badge/Streamlit-1.28+-FF4B4B?logo=streamlit&logoColor=white)](https://streamlit.io/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

**Advanced multi-stage continuous distillation column simulator** with NRTL thermodynamic model and optimized numerical solvers.

> 🎓 **Academic Origin**: Originally developed as a student project at ENSGTI (École Nationale Supérieure en Génie des Technologies Industrielles) in 2018. Refactored and modernized for production use with web interface and comprehensive documentation.

---

## ✨ Features

### 🔬 **Rigorous Scientific Models**
- **MESH Equations**: Material balance, Equilibrium, Summation, Heat balance
- **NRTL Model**: Non-Random Two-Liquid theory for non-ideal mixtures
- **Vapor-Liquid Equilibrium**: Activity coefficient calculations
- **Enthalpy Calculations**: Rigorous heat balances

### 🧮 **Advanced Numerical Methods**
- **Newton-Raphson** solver for non-linear systems
- **Block Tridiagonal Matrix** optimization (MRSL21 algorithm)
- **Full Matrix** solver (MRSL01) for comparison
- Analytical Jacobian with numerical perturbations

### 💻 **Modern Architecture**
- **High-Performance Backend**: Fortran 90 for computational efficiency
- **Python Wrapper**: Easy integration with modern workflows
- **Interactive Web UI**: Streamlit-based interface with real-time visualization
- **YAML Configuration**: Human-readable simulation setup

---

## 🚀 Quick Start

### Prerequisites

```bash
# System requirements
- gfortran (GCC Fortran compiler)
- Python 3.8+
- uv (Python package manager)
- Make

# Check installations
gfortran --version
python3 --version
uv --version
```

> 📦 **Using uv**: This project uses [uv](https://github.com/astral-sh/uv) for fast, reliable Python dependency management.
> Install with: `curl -LsSf https://astral.sh/uv/install.sh | sh` (see [docs/UV_GUIDE.md](docs/UV_GUIDE.md) for details)

### Installation

```bash
# Clone repository
git clone https://github.com/yourusername/distillation-column-simulator.git
cd distillation-column-simulator

# Install Python dependencies with uv (recommended)
uv sync

# Or use pip (alternative)
pip install -r frontend/requirements.txt

# Compile Fortran backend
cd backend
make
cd ..
```

### Run Web Interface

```bash
# With uv (recommended)
uv run streamlit run frontend/app.py

# Or activate venv first
source .venv/bin/activate  # Linux/macOS
streamlit run frontend/app.py
```

The application will open in your browser at `http://localhost:8501`

### Run from Command Line

```bash
# Compile backend
cd backend && make

# Edit configuration
nano ../config/simulation_config.yaml

# Run simulation
./distillation

# Results saved to RESULTS.csv
```

---

## 📊 Example: Acetone-Benzene-Chloroform Separation

```yaml
column:
  n_stages: 30
  pressure_atm: 1.0

feeds:
  - stage: 14
    flow_rate: 1.0
    composition:
      acetone: 0.6
      benzene: 0.3
      chloroform: 0.1

thermal:
  reboiler_duty: 21000.0    # cal/s
  condenser_duty: 15000.0   # cal/s
```

**System Features:**
- Ternary mixture with azeotrope (Acetone-Chloroform at 66/34%)
- Non-ideal behavior (NRTL model essential)
- 30 theoretical stages
- Typical convergence: ~1700 iterations, 50 seconds

---

## 📁 Project Structure

```
distillation-simulator/
├── backend/                  # Fortran computational engine
│   ├── src/
│   │   ├── modules/         # Global variable modules
│   │   ├── thermodynamics/  # NRTL, equilibrium, enthalpies
│   │   ├── solvers/         # MRSL01, MRSL21 linear solvers
│   │   └── utils/           # Utilities (norms, residuals)
│   └── Makefile             # Build system
│
├── python/                   # Python wrapper package
│   └── distillation_wrapper/
│       ├── fortran_interface.py
│       ├── config_manager.py
│       └── data_processor.py
│
├── frontend/                 # Streamlit web application
│   ├── app.py               # Main application
│   └── components/          # UI components
│
├── config/                   # Configuration files
│   ├── simulation_config.yaml
│   ├── components.yaml
│   └── examples/
│
├── docs/                     # Documentation
│   ├── scientific/          # Theory and equations
│   ├── technical/           # API and usage
│   └── original/            # Original 2018 project docs
│
└── results/                  # Simulation outputs
```

---

## 📚 Documentation

### Scientific Background
- [Thermodynamic Models](docs/scientific/thermodynamics.md) - NRTL theory and implementation
- [MESH Equations](docs/scientific/mesh_equations.md) - Material and energy balances
- [Numerical Methods](docs/scientific/numerical_methods.md) - Newton-Raphson and matrix solvers

### Technical Documentation
- [Installation Guide](docs/technical/installation.md)
- [Configuration Reference](docs/technical/configuration.md)
- [API Documentation](docs/technical/api_reference.md)
- [Python Wrapper Usage](docs/technical/python_wrapper.md)

### Original Project (2018)
- [Project Statement](docs/original/project_statement.md)
- [Full Technical Report](docs/original/rapport_distillation_continue.pdf) (39 pages, French)
- See branch `archive/ensgti-2018` for original unmodified code

---

## 🔧 Development

### Compile Backend

```bash
cd backend
make clean && make
```

### Run Tests

```bash
# Python tests
pytest tests/python/

# Fortran validation
cd tests/fortran && ./run_validation.sh
```

### Code Style

- **Fortran**: Fortran 90 standard, detailed comments
- **Python**: PEP 8, type hints, docstrings
- **Git**: Conventional commits

---

## 📈 Performance

| Solver  | Method              | Avg. Time | Iterations | Memory  |
|---------|---------------------|-----------|------------|---------|
| MRSL21  | Block Tridiagonal   | ~20s      | ~1700      | Low     |
| MRSL01  | Full Matrix         | ~26s      | ~1700      | Higher  |

*Benchmark: 30 stages, acetone-benzene-chloroform, convergence 10⁻⁶*

---

## 🎓 Academic Background

**Original Project (2018)**
- **Institution**: ENSGTI Pau, France
- **Course**: Modélisation des opérations unitaires II
- **Instructor**: Pr. Frédéric MARIAS
- **Student**: Younes AJEDDIG

**Modernization (2025)**
- Refactored for production use
- Added web interface
- Comprehensive documentation
- CI/CD integration

---

## 🤝 Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open Pull Request

---

## 📝 License

This project is licensed under the MIT License - see [LICENSE](LICENSE) file for details.

---

## 🙏 Acknowledgments

- **ENSGTI** for the original academic framework
- **Frédéric MARIAS** for scientific guidance (2018)
- Open-source community for tools and libraries

---

## 📧 Contact

**Younes AJEDDIG**
- GitHub: [@yajeddig](https://github.com/yajeddig)
- LinkedIn: [Younes AJEDDIG](https://www.linkedin.com/in/younes-ajeddig/)
- Email: younes.ajeddig@gmail.com
---

---

*Made with ❤️ for Process & Chemical Engineering*
