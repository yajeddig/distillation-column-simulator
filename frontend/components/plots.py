"""
Plotting Component

Renders interactive charts using Plotly.
"""

import streamlit as st
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import numpy as np


def render_plots(results_processor, detailed=False):
    """
    Render simulation result plots.
    
    Args:
        results_processor: ResultsProcessor instance with loaded data
        detailed: Show detailed multi-panel plots
    """
    
    # Get data
    temp_profile = results_processor.get_temperature_profile()
    liq_comp = results_processor.get_composition_profiles('liquid')
    vap_comp = results_processor.get_composition_profiles('vapor')
    flows = results_processor.get_flow_profiles()
    
    if not detailed:
        # Summary plots (2 columns)
        col1, col2 = st.columns(2)
        
        with col1:
            # Temperature profile
            fig_temp = go.Figure()
            fig_temp.add_trace(go.Scatter(
                x=temp_profile['stage'],
                y=temp_profile['temperature'],
                mode='lines+markers',
                name='Temperature',
                line=dict(color='red', width=3),
                marker=dict(size=6)
            ))
            fig_temp.update_layout(
                title="Temperature Profile",
                xaxis_title="Stage Number",
                yaxis_title="Temperature (K)",
                hovermode='x unified',
                height=400
            )
            st.plotly_chart(fig_temp, use_container_width=True)
        
        with col2:
            # Flow rates
            fig_flow = go.Figure()
            fig_flow.add_trace(go.Scatter(
                x=flows['stage'],
                y=flows['vapor_flow'],
                mode='lines',
                name='Vapor',
                line=dict(color='blue', width=2)
            ))
            fig_flow.add_trace(go.Scatter(
                x=flows['stage'],
                y=flows['liquid_flow'],
                mode='lines',
                name='Liquid',
                line=dict(color='green', width=2)
            ))
            fig_flow.update_layout(
                title="Flow Rate Profiles",
                xaxis_title="Stage Number",
                yaxis_title="Molar Flow Rate (mol/s)",
                hovermode='x unified',
                height=400
            )
            st.plotly_chart(fig_flow, use_container_width=True)
        
        # Composition profiles (full width)
        fig_comp = make_subplots(
            rows=1, cols=2,
            subplot_titles=("Liquid Composition", "Vapor Composition")
        )
        
        # Liquid compositions
        colors = ['#FF6B6B', '#4ECDC4', '#45B7D1']
        components = ['Acetone', 'Benzene', 'Chloroform']
        
        for i, (comp_name, color) in enumerate(zip(components, colors), 1):
            fig_comp.add_trace(
                go.Scatter(
                    x=liq_comp['stage'],
                    y=liq_comp[f'component_{i}'],
                    name=f'Liq {comp_name}',
                    line=dict(color=color, width=2)
                ),
                row=1, col=1
            )
            fig_comp.add_trace(
                go.Scatter(
                    x=vap_comp['stage'],
                    y=vap_comp[f'component_{i}'],
                    name=f'Vap {comp_name}',
                    line=dict(color=color, width=2, dash='dash')
                ),
                row=1, col=2
            )
        
        fig_comp.update_xaxes(title_text="Stage Number", row=1, col=1)
        fig_comp.update_xaxes(title_text="Stage Number", row=1, col=2)
        fig_comp.update_yaxes(title_text="Mole Fraction", row=1, col=1)
        fig_comp.update_yaxes(title_text="Mole Fraction", row=1, col=2)
        
        fig_comp.update_layout(
            height=500,
            hovermode='x unified',
            showlegend=True
        )
        
        st.plotly_chart(fig_comp, use_container_width=True)
    
    else:
        # Detailed view - all in one chart
        st.subheader("Detailed Column Profiles")
        
        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=(
                "Temperature Profile",
                "Flow Rates",
                "Liquid Composition",
                "Vapor Composition"
            ),
            vertical_spacing=0.12,
            horizontal_spacing=0.10
        )
        
        # Temperature
        fig.add_trace(
            go.Scatter(
                x=temp_profile['stage'],
                y=temp_profile['temperature'],
                mode='lines+markers',
                name='Temperature',
                line=dict(color='red', width=2)
            ),
            row=1, col=1
        )
        
        # Flows
        fig.add_trace(
            go.Scatter(
                x=flows['stage'],
                y=flows['vapor_flow'],
                name='Vapor Flow',
                line=dict(color='blue', width=2)
            ),
            row=1, col=2
        )
        fig.add_trace(
            go.Scatter(
                x=flows['stage'],
                y=flows['liquid_flow'],
                name='Liquid Flow',
                line=dict(color='green', width=2)
            ),
            row=1, col=2
        )
        
        # Compositions
        colors = ['#FF6B6B', '#4ECDC4', '#45B7D1']
        components = ['Acetone', 'Benzene', 'Chloroform']
        
        for i, (comp_name, color) in enumerate(zip(components, colors), 1):
            fig.add_trace(
                go.Scatter(
                    x=liq_comp['stage'],
                    y=liq_comp[f'component_{i}'],
                    name=comp_name,
                    line=dict(color=color, width=2),
                    showlegend=(i==1)
                ),
                row=2, col=1
            )
            fig.add_trace(
                go.Scatter(
                    x=vap_comp['stage'],
                    y=vap_comp[f'component_{i}'],
                    name=comp_name,
                    line=dict(color=color, width=2),
                    showlegend=False
                ),
                row=2, col=2
            )
        
        # Update axes
        fig.update_xaxes(title_text="Stage", row=1, col=1)
        fig.update_xaxes(title_text="Stage", row=1, col=2)
        fig.update_xaxes(title_text="Stage", row=2, col=1)
        fig.update_xaxes(title_text="Stage", row=2, col=2)
        
        fig.update_yaxes(title_text="T (K)", row=1, col=1)
        fig.update_yaxes(title_text="mol/s", row=1, col=2)
        fig.update_yaxes(title_text="Mole Fraction", row=2, col=1)
        fig.update_yaxes(title_text="Mole Fraction", row=2, col=2)
        
        fig.update_layout(
            height=800,
            showlegend=True,
            hovermode='x unified'
        )
        
        st.plotly_chart(fig, use_container_width=True)
