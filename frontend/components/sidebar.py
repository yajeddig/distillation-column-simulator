"""
Sidebar Component

Renders the configuration sidebar for column parameters.
"""

import streamlit as st


def render_sidebar():
    """
    Render sidebar with configuration inputs.
    
    Returns:
        Dictionary with configuration parameters
    """
    
    # Column configuration
    st.subheader("Column Design")
    
    n_stages = st.number_input(
        "Number of Stages",
        min_value=5,
        max_value=100,
        value=30,
        help="Total number of theoretical stages"
    )
    
    pressure = st.number_input(
        "Pressure (atm)",
        min_value=0.1,
        max_value=10.0,
        value=1.0,
        step=0.1,
        help="Operating pressure"
    )
    
    # Feed configuration
    st.subheader("Feed Stream")
    
    feed_stage = st.number_input(
        "Feed Stage",
        min_value=1,
        max_value=int(n_stages),
        value=int(n_stages//2),
        help="Stage number where feed enters"
    )
    
    feed_flow = st.number_input(
        "Feed Flow Rate (mol/s)",
        min_value=0.01,
        max_value=100.0,
        value=1.0,
        step=0.1
    )
    
    st.write("**Feed Composition** (mole fractions)")
    
    col1, col2 = st.columns(2)
    with col1:
        x_acetone = st.slider("Acetone", 0.0, 1.0, 0.6, 0.01)
        x_benzene = st.slider("Benzene", 0.0, 1.0, 0.3, 0.01)
    with col2:
        x_chloroform = 1.0 - x_acetone - x_benzene
        if x_chloroform < 0:
            st.error("⚠️ Sum of fractions exceeds 1.0!")
            x_chloroform = 0.0
        st.metric("Chloroform", f"{x_chloroform:.3f}")
    
    # Thermal duties
    st.subheader("Thermal Duties")
    
    reboiler_duty = st.number_input(
        "Reboiler Duty (cal/s)",
        min_value=1000.0,
        max_value=100000.0,
        value=21000.0,
        step=1000.0,
        help="Heat supplied to the reboiler"
    )
    
    condenser_duty = st.number_input(
        "Condenser Duty (cal/s)",
        min_value=1000.0,
        max_value=100000.0,
        value=15000.0,
        step=1000.0,
        help="Heat removed at the condenser"
    )
    
    # Build config dictionary
    config = {
        'column': {
            'n_stages': n_stages,
            'pressure_atm': pressure
        },
        'feeds': [
            {
                'stage': feed_stage,
                'flow_rate': feed_flow,
                'composition': {
                    'acetone': x_acetone,
                    'benzene': x_benzene,
                    'chloroform': x_chloroform
                },
                'state': 'liquid_at_bubble'
            }
        ],
        'thermal': {
            'reboiler_duty': reboiler_duty,
            'condenser_duty': condenser_duty
        },
        'liquid_sidestreams': [],
        'vapor_sidestreams': [],
        'solver': {
            'method': 'MRSL21',
            'tolerance': 1.0e-6,
            'relaxation': 0.01,
            'max_iterations': 5000
        },
        'output': {
            'results_file': 'results/simulation_results.csv',
            'verbose': True,
            'save_profiles': True
        }
    }
    
    return config
