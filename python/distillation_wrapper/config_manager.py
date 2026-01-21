"""
Configuration Manager

Handles reading/writing YAML configuration files
and converting to/from Fortran input format.
"""

import yaml
from pathlib import Path
from typing import Dict, Any, Optional


class ConfigManager:
    """
    Manages simulation configuration files.
    
    Supports:
    - YAML format (modern, human-readable)
    - Legacy .txt format (original Fortran input)
    - Conversion between formats
    """
    
    def __init__(self, config_dir: Optional[str] = None):
        """
        Initialize config manager.
        
        Args:
            config_dir: Path to config directory
        """
        if config_dir is None:
            current = Path(__file__).parent
            self.config_dir = current.parent.parent / "config"
        else:
            self.config_dir = Path(config_dir)
    
    def load_yaml(self, filename: str) -> Dict[str, Any]:
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
        
        Args:
            config: Configuration dictionary
            
        Returns:
            Formatted string for OPERATOIRE.txt
        """
        lines = []
        
        # Number of stages
        n_stages = config['column']['n_stages']
        lines.append(f"{n_stages}          Nombre d'étages théoriques")
        lines.append("———" * 30)
        
        # Feeds
        feeds = config.get('feeds', [])
        lines.append(f"{len(feeds)}           NOMBRE D'ALIMENTATION DANS LA COLONNE")
        lines.append("Donner les étages correspondants aux entrées (SUR LA MÊME LIGNE ESPACÉ PAR DES TABULATIONS)")
        
        feed_stages = '\t'.join(str(f['stage']) for f in feeds)
        lines.append(feed_stages)
        
        lines.append("DONNER LES DÉBIT D'ALIMENTATIONS CORRESPONDANTS SUR UNE MÊME LIGNE ESPACÉ PAR DES TABULATIONS [MOL/S]")
        feed_flows = '\t'.join(str(f['flow_rate']) for f in feeds)
        lines.append(feed_flows)
        
        lines.append("DONNER LES COMPOSITIONS DE CES ALIMENTATIONS EN LIGNE, ET CHAQUE ALIMENTATION UNE LIGNE")
        for feed in feeds:
            comp = feed['composition']
            comp_str = ' '.join(str(comp.get(k, 0)) for k in ['acetone', 'benzene', 'chloroform'])
            lines.append(comp_str)
        
        # Thermal duties
        lines.append("———" * 30)
        lines.append("Puissances")
        lines.append(f"{config['thermal']['reboiler_duty']}       Chaleur au bouilleur [cal/s]")
        lines.append(f"{config['thermal']['condenser_duty']}       Chaleur au condenseur[cal/s]")
        
        # Pressure
        lines.append("———" * 30)
        lines.append(f"{config['column']['pressure_atm']}           Pression de fonctionnement de la colonne [ATM]")
        
        # Liquid sidestreams
        lines.append("———" * 30)
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
        lines.append("———" * 30)
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
        
        lines.append("———" * 30)
        lines.append("")
        
        return '\n'.join(lines)
    
    def write_fortran_inputs(self, config: Dict[str, Any]):
        """
        Write Fortran-compatible input files from YAML config.
        
        Creates OPERATOIRE.txt and ALIMENTATION_PARAMETRE.txt
        """
        # Write OPERATOIRE.txt
        operatoire_content = self.yaml_to_fortran_operatoire(config)
        with open(self.config_dir / "OPERATOIRE.txt", 'w') as f:
            f.write(operatoire_content)
        
        print("✓ Created OPERATOIRE.txt from YAML config")
