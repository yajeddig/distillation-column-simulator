# Frontend - Distillation Column Simulator

Web-based interface for the distillation column simulator built with Streamlit.

## Features

- 🎛️ Interactive parameter configuration
- 📊 Real-time visualization with Plotly
- 📈 Temperature, composition, and flow profiles
- 📥 Export results to CSV
- 🔄 Live simulation execution

## Installation

```bash
cd frontend
pip install -r requirements.txt
```

## Usage

```bash
streamlit run app.py
```

## Components

- `app.py` - Main application
- `components/sidebar.py` - Configuration panel
- `components/plots.py` - Interactive charts
- `components/results.py` - Data tables

## Screenshots

*(Add screenshots here)*

## Development

```bash
# Watch mode (auto-reload)
streamlit run app.py --server.runOnSave true
```
