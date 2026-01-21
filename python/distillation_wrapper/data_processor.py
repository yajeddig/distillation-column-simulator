"""
Results Data Processor

Handles parsing and processing of simulation results
for analysis and visualization.
"""

import pandas as pd
import numpy as np
from pathlib import Path
from typing import Optional, Dict, List


class ResultsProcessor:
    """
    Process and analyze distillation simulation results.
    
    Handles:
    - CSV parsing
    - Data transformation
    - Profile extraction (T, compositions, flows)
    """
    
    def __init__(self, results_file: Optional[str] = None):
        """
        Initialize results processor.
        
        Args:
            results_file: Path to RESULTS.csv file
        """
        if results_file is None:
            # Try to find results file
            possible_paths = [
                Path("RESULTS.csv"),
                Path("../backend/RESULTS.csv"),
                Path("results/simulation_results.csv"),
            ]
            for p in possible_paths:
                if p.exists():
                    self.results_file = p
                    break
            else:
                self.results_file = None
        else:
            self.results_file = Path(results_file)
        
        self.df = None
    
    def load(self, results_file: Optional[str] = None) -> pd.DataFrame:
        """
        Load results CSV file.
        
        Args:
            results_file: Path to results file (optional if set in __init__)
            
        Returns:
            DataFrame with simulation results
        """
        if results_file:
            self.results_file = Path(results_file)
        
        if not self.results_file or not self.results_file.exists():
            raise FileNotFoundError(f"Results file not found: {self.results_file}")
        
        # Read CSV (handle Fortran formatting)
        self.df = pd.read_csv(
            self.results_file,
            sep=';',
            skipinitialspace=True
        )
        
        # Clean column names
        self.df.columns = self.df.columns.str.strip()
        
        # Convert numeric columns
        for col in self.df.columns:
            if col != 'ÉTAGE PLATEAU':
                self.df[col] = pd.to_numeric(self.df[col], errors='coerce')
        
        return self.df
    
    def get_temperature_profile(self) -> Dict[str, np.ndarray]:
        """
        Extract temperature profile along the column.
        
        Returns:
            Dictionary with 'stage' and 'temperature' arrays
        """
        if self.df is None:
            self.load()
        
        return {
            'stage': self.df['ÉTAGE PLATEAU'].values,
            'temperature': self.df['TEMPERATURE'].values
        }
    
    def get_composition_profiles(self, phase: str = 'liquid') -> Dict[str, np.ndarray]:
        """
        Extract composition profiles.
        
        Args:
            phase: 'liquid' or 'vapor'
            
        Returns:
            Dictionary with stage numbers and composition arrays
        """
        if self.df is None:
            self.load()
        
        prefix = 'COMPO LIQUIDE' if phase == 'liquid' else 'COMPO VAPEUR'
        
        # Find composition columns
        comp_cols = [col for col in self.df.columns if prefix in col]
        
        profiles = {'stage': self.df['ÉTAGE PLATEAU'].values}
        
        for i, col in enumerate(comp_cols, 1):
            profiles[f'component_{i}'] = self.df[col].values
        
        return profiles
    
    def get_flow_profiles(self) -> Dict[str, np.ndarray]:
        """
        Extract vapor and liquid flow rate profiles.
        
        Returns:
            Dictionary with stage, vapor_flow, liquid_flow arrays
        """
        if self.df is None:
            self.load()
        
        return {
            'stage': self.df['ÉTAGE PLATEAU'].values,
            'vapor_flow': self.df['DEBIT VAPEUR'].values,
            'liquid_flow': self.df['DEBIT LIQUIDE'].values
        }
    
    def summary(self) -> Dict:
        """
        Generate summary statistics.
        
        Returns:
            Dictionary with key metrics
        """
        if self.df is None:
            self.load()
        
        temp_profile = self.get_temperature_profile()
        
        return {
            'n_stages': len(self.df),
            'min_temperature': temp_profile['temperature'].min(),
            'max_temperature': temp_profile['temperature'].max(),
            'top_temperature': temp_profile['temperature'][0],
            'bottom_temperature': temp_profile['temperature'][-1],
            'max_vapor_flow': self.df['DEBIT VAPEUR'].max(),
            'max_liquid_flow': self.df['DEBIT LIQUIDE'].max(),
        }
    
    def to_dict(self) -> Dict:
        """Convert results to nested dictionary."""
        if self.df is None:
            self.load()
        
        return self.df.to_dict(orient='records')
