"""
Distillation Column Simulator - Web Interface

Interactive Streamlit application for running and visualizing
distillation column simulations.

Usage:
    streamlit run app.py
"""

import streamlit as st
import sys
from pathlib import Path

# Add python package to path
sys.path.insert(0, str(Path(__file__).parent.parent / "python"))

from distillation_wrapper import DistillationSimulator, ConfigManager, ResultsProcessor
from components.sidebar import render_sidebar
from components.plots import render_plots
from components.results import render_results_table

# Page configuration
st.set_page_config(
    page_title="Distillation Column Simulator",
    page_icon="🧪",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Title and description
st.title("🧪 Multi-Stage Distillation Column Simulator")
st.markdown("""
Advanced simulation of continuous distillation columns with NRTL thermodynamic model.
Configure your column parameters in the sidebar and run simulations with optimized numerical solvers.
""")

# Initialize session state
if 'simulation_results' not in st.session_state:
    st.session_state.simulation_results = None
if 'simulation_metrics' not in st.session_state:
    st.session_state.simulation_metrics = None

# Sidebar configuration
with st.sidebar:
    st.header("⚙️ Configuration")
    config = render_sidebar()
    
    st.divider()
    
    # Simulation controls
    st.subheader("🎯 Run Simulation")
    
    solver_method = st.selectbox(
        "Solver Method",
        ["MRSL21", "MRSL01"],
        help="MRSL21: Block tridiagonal (faster), MRSL01: Full matrix"
    )
    
    run_button = st.button("▶️ Run Simulation", type="primary", use_container_width=True)

# Main content area
tab1, tab2, tab3, tab4 = st.tabs(["📊 Results", "📈 Profiles", "📋 Data Table", "ℹ️ About"])

with tab1:
    if st.session_state.simulation_metrics:
        # Display metrics
        col1, col2, col3, col4 = st.columns(4)
        
        metrics = st.session_state.simulation_metrics
        
        with col1:
            st.metric("Iterations", metrics.get('iterations', 'N/A'))
        with col2:
            st.metric("Final Residual", f"{metrics.get('final_residual', 0):.2e}")
        with col3:
            st.metric("Time (s)", f"{metrics.get('elapsed_time', 0):.2f}")
        with col4:
            status = "✅ Converged" if metrics.get('success') else "❌ Failed"
            st.metric("Status", status)
        
        st.divider()
        
        # Plots
        if st.session_state.simulation_results:
            render_plots(st.session_state.simulation_results)
    else:
        st.info("👈 Configure parameters in the sidebar and click 'Run Simulation' to start")

with tab2:
    if st.session_state.simulation_results:
        st.subheader("Temperature and Composition Profiles")
        render_plots(st.session_state.simulation_results, detailed=True)
    else:
        st.info("No simulation data available. Run a simulation first.")

with tab3:
    if st.session_state.simulation_results:
        render_results_table(st.session_state.simulation_results)
    else:
        st.info("No simulation data available. Run a simulation first.")

with tab4:
    st.markdown("""
    ## About This Simulator
    
    This application simulates multi-stage continuous distillation columns using:
    
    ### 🔬 Scientific Models
    - **MESH Equations**: Material balance, Equilibrium, Summation, Heat balance
    - **Thermodynamics**: NRTL (Non-Random Two-Liquid) model for non-ideal mixtures
    - **Vapor-Liquid Equilibrium**: Raoult's law with activity coefficients
    
    ### 🧮 Numerical Methods
    - **Newton-Raphson** method for non-linear equation solving
    - **Block Tridiagonal Matrix** optimization (MRSL21 algorithm)
    - Analytical Jacobian with numerical perturbations
    
    ### 💻 Implementation
    - **Backend**: Fortran 90 (high-performance computing)
    - **Frontend**: Python + Streamlit (modern web interface)
    - **Visualization**: Plotly (interactive charts)
    
    ### 📚 Academic Background
    Original implementation: **ENSGTI 2018** student project  
    Course: *Modélisation des opérations unitaires II*  
    Institution: École Nationale Supérieure en Génie des Technologies Industrielles
    
    ### 🔗 Links
    - [GitHub Repository](https://github.com/yourusername/distillation-simulator)
    - [Documentation](https://github.com/yourusername/distillation-simulator/docs)
    - [Technical Report](https://github.com/yourusername/distillation-simulator/docs/original)
    """)

# Run simulation logic
if run_button:
    with st.spinner("🔄 Running simulation..."):
        try:
            # Initialize simulator
            simulator = DistillationSimulator()
            config_manager = ConfigManager()
            
            # Write config files (convert from UI to Fortran format)
            config_manager.write_fortran_inputs(config)
            
            # Compile if needed
            if not simulator.executable.exists():
                st.info("Compiling Fortran code...")
                if not simulator.compile():
                    st.error("❌ Compilation failed!")
                    st.stop()
            
            # Run simulation
            metrics = simulator.run(method=solver_method)
            
            # Load results
            processor = ResultsProcessor()
            results_df = processor.load(simulator.get_results_path())
            
            # Store in session state
            st.session_state.simulation_metrics = metrics
            st.session_state.simulation_results = processor
            
            st.success(f"✅ Simulation completed! Converged in {metrics['iterations']} iterations")
            st.rerun()
            
        except Exception as e:
            st.error(f"❌ Simulation failed: {str(e)}")
            st.exception(e)
