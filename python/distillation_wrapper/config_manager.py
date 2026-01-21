"""
Configuration Manager

Handles reading/writing YAML configuration files
and converting to/from Fortran input format.
"""

import yaml
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List


class ConfigManager:
    """
    Manages simulation configuration files.
    
    Supports:
    - YAML format (modern, human-readable)
    - Legacy .txt format (original Fortran input)
    - Automatic conversion YAML -> TXT for Fortran execution
    """
    
    # Component order for Fortran (must match MODULE_PARAMETRE_THERMO.f90)
    COMPONENTS = ['acetone', 'benzene', 'chloroform']
    
    def __init__(self, config_dir: Optional[str] = None, backend_dir: Optional[str] = None):
        """
        Initialize config manager.
        
        Args:
            config_dir: Path to config directory
            backend_dir: Path to backend directory (for Fortran files)
        """
        if config_dir is None:
            current = Path(__file__).parent
            self.config_dir = current.parent.parent / "config"
        else:
            self.config_dir = Path(config_dir)
            
        if backend_dir is None:
            current = Path(__file__).parent
            self.backend_dir = current.parent.parent / "backend"
        else:
            self.backend_dir = Path(backend_dir)
    
    def load_yaml(self, filename: str = "simulation_config.yaml") -> Dict[str, Any]:
        """
        Load YAML configuration file.
        
        Args:
            filename: Config file name (with or without .yaml extension)
            
        Returns:
            Configuration dictionary
        """
        if not filename.endswith('.yaml'):
            filename += '.yaml'
        
        filepath = self.config_dir / filename
        
        with open(filepath, 'r') as f:
            config = yaml.safe_load(f)
        
        return config
    
    def save_yaml(self, config: Dict[str, Any], filename: str):
        """Save configuration to YAML file."""
        if not filename.endswith('.yaml'):
            filename += '.yaml'
        
        filepath = self.config_dir / filename
        
        with open(filepath, 'w') as f:
            yaml.dump(config, f, default_flow_style=False, sort_keys=False)
    
    def yaml_to_fortran_operatoire(self, config: Dict[str, Any]) -> str:
        """
        Convert YAML config to Fortran OPERATOIRE.txt format.
        
        The format must match exactly what the Fortran code expects:
        - Line 1: N (number of stages)
        - Line 2: separator
        - Line 3: NB_ALIM (number of feeds)
        - Line 4: comment
        - Line 5: feed stages (tab-separated)
        - etc.
        
        Args:
            config: Configuration dictionary from YAML
            
        Returns:
            Formatted string for OPERATOIRE.txt
        """
        lines = []
        sep = "—" * 90
        
        # Number of stages
        n_stages = config['column']['n_stages']
        lines.append(f"{n_stages}          Nombre d'étages théoriques")
        lines.append(sep)
        
        # Feeds
        feeds = config.get('feeds', [])
        lines.append(f"{len(feeds)}           NOMBRE D'ALIMENTATION DANS LA COLONNE")
        lines.append("Donner les étages correspondants aux entrées (SUR LA MÊME LIGNE ESPACÉ PAR DES TABULATIONS)")
        
        if feeds:
            feed_stages = '\t'.join(str(f['stage']) for f in feeds)
            lines.append(feed_stages)
            lines.append("DONNER LES DÉBIT D'ALIMENTATIONS CORRESPONDANTS SUR UNE MÊME LIGNE ESPACÉ PAR DES TABULATIONS [MOL/S]")
            feed_flows = '\t'.join(str(f['flow_rate']) for f in feeds)
            lines.append(feed_flows)
            lines.append("DONNER LES COMPOSITIONS DE CES ALIMENTATIONS EN LIGNE, ET CHAQUE ALIMENTATION UNE LIGNE")
            for feed in feeds:
                comp = feed['composition']
                # Must be in order: acetone, benzene, chloroform
                comp_values = [comp.get(c, 0.0) for c in self.COMPONENTS]
                comp_str = ' '.join(str(v) for v in comp_values)
                lines.append(comp_str)
        else:
            lines.append("")
            lines.append("DONNER LES DÉBIT D'ALIMENTATIONS CORRESPONDANTS SUR UNE MÊME LIGNE ESPACÉ PAR DES TABULATIONS [MOL/S]")
            lines.append("")
            lines.append("DONNER LES COMPOSITIONS DE CES ALIMENTATIONS EN LIGNE, ET CHAQUE ALIMENTATION UNE LIGNE")
        
        # Thermal duties
        lines.append(sep)
        lines.append("Puissances")
        lines.append(f"{config['thermal']['reboiler_duty']}       Chaleur au bouilleur [cal/s]")
        lines.append(f"{config['thermal']['condenser_duty']}       Chaleur au condenseur[cal/s]")
        
        # Pressure
        lines.append(sep)
        lines.append(f"{config['column']['pressure_atm']}           Pression de fonctionnement de la colonne [ATM]")
        
        # Liquid sidestreams
        lines.append(sep)
        liquid_ss = config.get('liquid_sidestreams', [])
        lines.append(f"{len(liquid_ss)} Nombre de soutirage liquide")
        lines.append("LISTE DES ETAGES DE SOUTIRAGE SUR LA MÊME LIGNE (TABULATION ENTRE CHAQUE VALEUR)")
        if liquid_ss:
            lines.append('\t'.join(str(s['stage']) for s in liquid_ss))
            lines.append("DEBIT DE SOUTIRAGE LIQUIDE [MOL/S] SUR LA MÊME LIGNE (TABULATION ENTRE CHAQUE VALEUR)")
            lines.append('\t'.join(str(s['flow_rate']) for s in liquid_ss))
        else:
            lines.append("")
            lines.append("")
            lines.append("")
        
        # Vapor sidestreams
        lines.append(sep)
        vapor_ss = config.get('vapor_sidestreams', [])
        lines.append(f"{len(vapor_ss)} Nombre de soutirage vapeur")
        lines.append("LISTE DES ETAGES DE SOUTIRAGE VAPEUR SUR LA MÊME LIGNE (TABULATION ENTRE CHAQUE VALEUR)")
        if vapor_ss:
            lines.append('\t'.join(str(s['stage']) for s in vapor_ss))
            lines.append("DONNER DEBIT DE SOUTIRAGE VAPEUR [MOL/S] SUR LA MÊME LIGNE (TABULATION ENTRE CHAQUE VALEUR)")
            lines.append('\t'.join(str(s['flow_rate']) for s in vapor_ss))
        else:
            lines.append("")
            lines.append("")
            lines.append("")
        
        lines.append(sep)
        lines.append("")
        
        return '\n'.join(lines)
    
    def write_fortran_inputs(self, config: Dict[str, Any], to_backend: bool = True):
        """
        Write Fortran-compatible input files from YAML config.
        
        Creates OPERATOIRE.txt in both config/ and backend/ directories.
        
        Args:
            config: Configuration dictionary
            to_backend: If True, also write to backend directory
        """
        operatoire_content = self.yaml_to_fortran_operatoire(config)
        
        # Write to config directory
        with open(self.config_dir / "OPERATOIRE.txt", 'w') as f:
            f.write(operatoire_content)
        
        # Also write to backend for Fortran execution
        if to_backend:
            with open(self.backend_dir / "OPERATOIRE.txt", 'w') as f:
                f.write(operatoire_content)
        
        return operatoire_content
    
    def sync_yaml_to_fortran(self, yaml_file: str = "simulation_config.yaml"):
        """
        Load YAML config and write Fortran input files.
        
        This is the main method to call before running a simulation
        when using YAML configuration.
        
        Args:
            yaml_file: Name of YAML config file
            
        Returns:
            Configuration dictionary
        """
        config = self.load_yaml(yaml_file)
        self.write_fortran_inputs(config)
        
        # Also copy ALIMENTATION_PARAMETRE.txt to backend if it exists
        alim_src = self.config_dir / "ALIMENTATION_PARAMETRE.txt"
        alim_dst = self.backend_dir / "ALIMENTATION_PARAMETRE.txt"
        if alim_src.exists():
            shutil.copy2(alim_src, alim_dst)
        
        return config
    
    def create_config_from_ui(
        self,
        n_stages: int,
        pressure: float,
        feed_stage: int,
        feed_flow: float,
        composition: Dict[str, float],
        reboiler_duty: float,
        condenser_duty: float,
        liquid_sidestreams: List[Dict] = None,
        vapor_sidestreams: List[Dict] = None,
        solver_method: str = "MRSL21"
    ) -> Dict[str, Any]:
        """
        Create configuration dictionary from UI parameters.
        
        Args:
            n_stages: Number of theoretical stages
            pressure: Operating pressure [atm]
            feed_stage: Feed stage number
            feed_flow: Feed molar flow rate [mol/s]
            composition: Dict with component mole fractions
            reboiler_duty: Reboiler heat duty [cal/s]
            condenser_duty: Condenser heat duty [cal/s]
            liquid_sidestreams: List of liquid sidestream dicts
            vapor_sidestreams: List of vapor sidestream dicts
            solver_method: MRSL01 or MRSL21
            
        Returns:
            Configuration dictionary
        """
        config = {
            'column': {
                'n_stages': n_stages,
                'pressure_atm': pressure
            },
            'feeds': [{
                'stage': feed_stage,
                'flow_rate': feed_flow,
                'composition': composition,
                'state': 'liquid_at_bubble'
            }],
            'thermal': {
                'reboiler_duty': reboiler_duty,
                'condenser_duty': condenser_duty
            },
            'liquid_sidestreams': liquid_sidestreams or [],
            'vapor_sidestreams': vapor_sidestreams or [],
            'solver': {
                'method': solver_method,
                'tolerance': 1.0e-6,
                'max_iterations': 5000
            }
        }
        
        return config
