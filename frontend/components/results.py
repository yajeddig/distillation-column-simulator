"""
Results Table Component

Displays simulation results in tabular format.
"""

import streamlit as st


def render_results_table(results_processor):
    """
    Render results data table with download option.
    
    Args:
        results_processor: ResultsProcessor instance with loaded data
    """
    
    st.subheader("Simulation Results Data")
    
    # Get data
    df = results_processor.df
    
    # Display summary
    summary = results_processor.summary()
    
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("Number of Stages", summary['n_stages'])
    with col2:
        st.metric("Top Temperature (K)", f"{summary['top_temperature']:.2f}")
    with col3:
        st.metric("Bottom Temperature (K)", f"{summary['bottom_temperature']:.2f}")
    
    st.divider()
    
    # Data table with formatting
    st.dataframe(
        df.style.format({
            'DEBIT VAPEUR': '{:.4f}',
            'DEBIT LIQUIDE': '{:.4f}',
            'TEMPERATURE': '{:.2f}',
        }),
        use_container_width=True,
        height=500
    )
    
    # Download button
    csv = df.to_csv(index=False)
    st.download_button(
        label="📥 Download Results (CSV)",
        data=csv,
        file_name="distillation_results.csv",
        mime="text/csv",
        use_container_width=True
    )
