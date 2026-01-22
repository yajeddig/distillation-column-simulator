"""
Components Configuration Component

Renders the thermodynamic parameters configuration tab
for editing component properties and NRTL binary interaction parameters.
"""

import streamlit as st
from typing import Dict, Any


def render_components_config() -> Dict[str, Any]:
    """
    Render the components configuration interface.
    
    Returns:
        Dictionary with updated component parameters
    """
    
    st.subheader("🧪 Component Properties")
    
    # Component selection tabs
    comp_tab1, comp_tab2, comp_tab3 = st.tabs(["Acetone", "Benzene", "Chloroform"])
    
    components = {}
    
    with comp_tab1:
        components['acetone'] = render_component_form(
            "acetone",
            default_tb=329.44,
            default_hvap=29600,
            default_cp_vap=80.5,
            default_cp_liq=134.0,
            default_antoine=[69.006, -5599.6, -7.0985, 6.2237e-6, 2.0]
        )
    
    with comp_tab2:
        components['benzene'] = render_component_form(
            "benzene",
            default_tb=353.24,
            default_hvap=30800,
            default_cp_vap=96.0,
            default_cp_liq=147.0,
            default_antoine=[83.107, -6486.2, -9.2194, 6.9844e-6, 2.0]
        )
    
    with comp_tab3:
        components['chloroform'] = render_component_form(
            "chloroform",
            default_tb=334.33,
            default_hvap=29500,
            default_cp_vap=68.9,
            default_cp_liq=117.0,
            default_antoine=[146.43, -7792.3, -20.614, 0.024578, 1.0]
        )
    
    st.divider()
    
    # NRTL Parameters
    st.subheader("🔗 NRTL Binary Interaction Parameters")
    
    st.info("""
    **NRTL Model**: Non-Random Two-Liquid model for non-ideal liquid mixtures.
    - **τᵢⱼ (Cij)**: Energy interaction parameters [cal/mol]
    - **αᵢⱼ**: Non-randomness parameter (typically 0.2-0.47)
    """)
    
    nrtl = {}
    
    col1, col2, col3 = st.columns(3)
    
    with col1:
        st.markdown("**Acetone - Benzene**")
        nrtl['acetone-benzene'] = {
            'C12': st.number_input("C₁₂ (cal/mol)", value=-193.340, key="c12_ab", format="%.3f"),
            'C21': st.number_input("C₂₁ (cal/mol)", value=569.931, key="c21_ab", format="%.3f"),
            'alpha': st.number_input("α₁₂", value=0.3007, key="alpha_ab", format="%.4f", 
                                     min_value=0.0, max_value=1.0)
        }
    
    with col2:
        st.markdown("**Acetone - Chloroform**")
        nrtl['acetone-chloroform'] = {
            'C12': st.number_input("C₁₃ (cal/mol)", value=-643.277, key="c12_ac", format="%.3f"),
            'C21': st.number_input("C₃₁ (cal/mol)", value=228.457, key="c21_ac", format="%.3f"),
            'alpha': st.number_input("α₁₃", value=0.3043, key="alpha_ac", format="%.4f",
                                     min_value=0.0, max_value=1.0)
        }
    
    with col3:
        st.markdown("**Benzene - Chloroform**")
        nrtl['benzene-chloroform'] = {
            'C12': st.number_input("C₂₃ (cal/mol)", value=0.0, key="c12_bc", format="%.3f"),
            'C21': st.number_input("C₃₂ (cal/mol)", value=0.0, key="c21_bc", format="%.3f"),
            'alpha': st.number_input("α₂₃", value=0.0, key="alpha_bc", format="%.4f",
                                     min_value=0.0, max_value=1.0)
        }
    
    return {
        'components': components,
        'nrtl_parameters': nrtl
    }


def render_component_form(
    name: str,
    default_tb: float,
    default_hvap: float,
    default_cp_vap: float,
    default_cp_liq: float,
    default_antoine: list
) -> Dict[str, Any]:
    """
    Render form for a single component's properties.
    
    Args:
        name: Component name
        default_*: Default values for properties
        
    Returns:
        Dictionary with component properties
    """
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("**Physical Properties**")
        tb = st.number_input(
            "Boiling Point (K)", 
            value=default_tb, 
            key=f"tb_{name}",
            format="%.2f",
            help="Normal boiling point at 1 atm"
        )
        hvap = st.number_input(
            "Heat of Vaporization (J/mol)", 
            value=default_hvap, 
            key=f"hvap_{name}",
            format="%.0f",
            help="Enthalpy of vaporization at boiling point"
        )
    
    with col2:
        st.markdown("**Heat Capacities (J/mol·K)**")
        cp_vap = st.number_input(
            "Cp Vapor", 
            value=default_cp_vap, 
            key=f"cpv_{name}",
            format="%.1f"
        )
        cp_liq = st.number_input(
            "Cp Liquid", 
            value=default_cp_liq, 
            key=f"cpl_{name}",
            format="%.1f"
        )
    
    # Antoine coefficients in expander
    with st.expander("📊 Antoine Equation Coefficients"):
        st.latex(r"\log_{10}(P_{sat}) = A + \frac{B}{T} + C \cdot \log_{10}(T) + D \cdot T^E")
        
        c1, c2, c3, c4, c5 = st.columns(5)
        antoine = [
            c1.number_input("A", value=default_antoine[0], key=f"ant_a_{name}", format="%.4f"),
            c2.number_input("B", value=default_antoine[1], key=f"ant_b_{name}", format="%.2f"),
            c3.number_input("C", value=default_antoine[2], key=f"ant_c_{name}", format="%.4f"),
            c4.number_input("D", value=default_antoine[3], key=f"ant_d_{name}", format="%.6e"),
            c5.number_input("E", value=default_antoine[4], key=f"ant_e_{name}", format="%.1f")
        ]
    
    return {
        'boiling_point': tb,
        'heat_of_vaporization': hvap,
        'heat_capacity': {
            'vapor': cp_vap,
            'liquid': cp_liq
        },
        'antoine_coefficients': {
            'A': antoine[0],
            'B': antoine[1],
            'C': antoine[2],
            'D': antoine[3],
            'E': antoine[4]
        }
    }


def components_to_fortran_format(config: Dict[str, Any]) -> str:
    """
    Convert components config to ALIMENTATION_PARAMETRE.txt format.
    
    Args:
        config: Configuration dictionary with components and nrtl_parameters
        
    Returns:
        Formatted string for ALIMENTATION_PARAMETRE.txt
    """
    lines = []
    sep = "—" * 48
    
    comps = config['components']
    nrtl = config['nrtl_parameters']
    
    # Number of components
    lines.append("3       NOMBRE DE CONSTITUANTS")
    lines.append(sep)
    
    # Boiling points
    for i, name in enumerate(['acetone', 'benzene', 'chloroform'], 1):
        tb = comps[name]['boiling_point']
        lines.append(f"{tb}    T EBULLITION {i} [K]")
    lines.append(sep)
    
    # Heat of vaporization
    for i, name in enumerate(['acetone', 'benzene', 'chloroform'], 1):
        hvap = comps[name]['heat_of_vaporization']
        lines.append(f"{hvap:.2E}    H VAP{i} [J/MOL]".replace('E', 'D'))
    lines.append(sep)
    
    # Cp vapor
    for i, name in enumerate(['acetone', 'benzene', 'chloroform'], 1):
        cpv = comps[name]['heat_capacity']['vapor']
        lines.append(f"{cpv:.2E}    CP VAP{i} [J/MOL/K]".replace('E', 'D'))
    lines.append(sep)
    
    # Cp liquid
    for i, name in enumerate(['acetone', 'benzene', 'chloroform'], 1):
        cpl = comps[name]['heat_capacity']['liquid']
        lines.append(f"{cpl:.2E}    CP LIQUIDE{i} [J/MOL/K]".replace('E', 'D'))
    lines.append(sep)
    
    # Antoine coefficients (5 lines, each with 3 values)
    lines.append("COEFFICIENT PSAT")
    antoine_keys = ['A', 'B', 'C', 'D', 'E']
    for key in antoine_keys:
        vals = [comps[name]['antoine_coefficients'][key] for name in ['acetone', 'benzene', 'chloroform']]
        if key == 'D':
            lines.append(' '.join(f"{v:.4E}".replace('E', 'D') for v in vals))
        else:
            lines.append(' '.join(f"{v}" for v in vals))
    lines.append(sep)
    
    # NRTL Cij matrix
    lines.append("COEFFICIENT Cij")
    # Row 1: 0, C12(ab), C13(ac)
    lines.append(f"0. {nrtl['acetone-benzene']['C12']} {nrtl['acetone-chloroform']['C12']}")
    # Row 2: C21(ab), 0, C23(bc)
    lines.append(f"{nrtl['acetone-benzene']['C21']} 0. {nrtl['benzene-chloroform']['C12']}")
    # Row 3: C31(ac), C32(bc), 0
    lines.append(f"{nrtl['acetone-chloroform']['C21']} {nrtl['benzene-chloroform']['C21']} 0.")
    lines.append(sep)
    
    # NRTL alpha matrix
    lines.append("COEFFICIENT ALPHAij")
    lines.append(f"0. {nrtl['acetone-benzene']['alpha']} {nrtl['acetone-chloroform']['alpha']}")
    lines.append(f"{nrtl['acetone-benzene']['alpha']} 0. {nrtl['benzene-chloroform']['alpha']}")
    lines.append(f"{nrtl['acetone-chloroform']['alpha']} {nrtl['benzene-chloroform']['alpha']} 0.")
    lines.append(sep)
    
    # Component names
    lines.append("1 ACETONE")
    lines.append("2 BENZENE")
    lines.append("3 CHLOROFORME")
    lines.append("")
    
    return '\n'.join(lines)
