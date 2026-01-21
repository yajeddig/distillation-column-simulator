"""
Fortran Interface Module

Handles execution of the Fortran distillation simulator
and communication via file I/O.
"""

import subprocess
import shutil
import os
from pathlib import Path
from typing import Dict, Optional, Any
import time

from .config_manager import ConfigManager


class DistillationSimulator:
    """
    Python wrapper for the Fortran distillation column simulator.
    
    This class manages:
    - Compilation of Fortran code
    - Execution of simulations
    - Input/output file handling
    - YAML config to Fortran format conversion
    """
    
    def __init__(self, backend_dir: Optional[str] = None):
        """
        Initialize the simulator.
        
        Args:
            backend_dir: Path to backend directory (default: auto-detect)
        """
        if backend_dir is None:
            # Auto-detect backend directory
            current = Path(__file__).parent
            self.backend_dir = current.parent.parent / "backend"
        else:
            self.backend_dir = Path(backend_dir)
        
        self.executable = self.backend_dir / "distillation"
        self.config_dir = self.backend_dir.parent / "config"
        self.results_dir = self.backend_dir.parent / "results"
        
        # Initialize config manager
        self.config_manager = ConfigManager(
            config_dir=str(self.config_dir),
            backend_dir=str(self.backend_dir)
        )
        
    def _sync_config_files(self):
        """Copy config files from config/ to backend/ for Fortran execution."""
        config_files = ["OPERATOIRE.txt", "ALIMENTATION_PARAMETRE.txt"]
        for filename in config_files:
            src = self.config_dir / filename
            dst = self.backend_dir / filename
            if src.exists():
                shutil.copy2(src, dst)
    
    def load_yaml_config(self, yaml_file: str = "simulation_config.yaml") -> Dict[str, Any]:
        """
        Load YAML config and convert to Fortran format.
        
        Args:
            yaml_file: Name of YAML config file
            
        Returns:
            Configuration dictionary
        """
        return self.config_manager.sync_yaml_to_fortran(yaml_file)
    
    def set_config(self, config: Dict[str, Any]):
        """
        Set simulation configuration from a dictionary.
        
        Converts the config to Fortran format and writes input files.
        
        Args:
            config: Configuration dictionary (YAML format)
        """
        self.config_manager.write_fortran_inputs(config, to_backend=True)
        
    def compile(self, force: bool = False) -> bool:
        """
        Compile the Fortran source code.
        
        Args:
            force: Force recompilation even if executable exists
            
        Returns:
            True if compilation successful
        """
        if self.executable.exists() and not force:
            print(f"✓ Executable already exists: {self.executable}")
            return True
        
        print("Compiling Fortran code...")
        try:
            result = subprocess.run(
                ["make", "clean", "all"],
                cwd=self.backend_dir,
                capture_output=True,
                text=True,
                check=True
            )
            print("✓ Compilation successful")
            return True
        except subprocess.CalledProcessError as e:
            print(f"✗ Compilation failed:\n{e.stderr}")
            return False
    
    def run(self, method: str = "MRSL21", timeout: int = 300) -> Dict:
        """
        Run the distillation simulation.
        
        Args:
            method: Solver method ("MRSL01" or "MRSL21")
            timeout: Maximum execution time in seconds
            
        Returns:
            Dictionary with simulation results and metadata
        """
        if not self.executable.exists():
            print("Executable not found. Compiling...")
            if not self.compile():
                raise RuntimeError("Compilation failed")
        
        # Ensure config files are up to date
        self._sync_config_files()
        
        # Prepare input (method choice: 1=MRSL01, 2=MRSL21)
        method_input = "2\n" if method == "MRSL21" else "1\n"
        
        print(f"Running simulation with {method} solver...")
        start_time = time.time()
        
        try:
            result = subprocess.run(
                [str(self.executable)],
                cwd=self.backend_dir,
                input=method_input,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=True
            )
            
            elapsed_time = time.time() - start_time
            
            # Parse output for key metrics
            output_lines = result.stdout.split('\n')
            metrics = self._parse_output(output_lines)
            metrics['elapsed_time'] = elapsed_time
            metrics['method'] = method
            
            print(f"✓ Simulation complete ({elapsed_time:.2f}s)")
            return metrics
            
        except subprocess.TimeoutExpired:
            raise RuntimeError(f"Simulation timeout ({timeout}s)")
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Simulation failed:\n{e.stderr}")
    
    def _parse_output(self, lines: list) -> Dict:
        """Parse Fortran output to extract metrics."""
        metrics = {
            'iterations': None,
            'final_residual': None,
            'success': False
        }
        
        for line in lines:
            if 'NOMBRE ITERATION' in line:
                try:
                    metrics['iterations'] = int(line.split(':')[1].strip())
                except:
                    pass
            elif 'NORME RESIDU   FINAL' in line:
                try:
                    metrics['final_residual'] = float(line.split(':')[1].strip())
                except:
                    pass
            elif 'FIN DE LA RESOLUTION' in line:
                metrics['success'] = True
        
        return metrics
    
    def get_results_path(self) -> Path:
        """Get path to results CSV file."""
        # Check in backend directory first (original location)
        backend_results = self.backend_dir / "RESULTS.csv"
        if backend_results.exists():
            return backend_results
        
        # Check in results directory
        results_csv = self.results_dir / "simulation_results.csv"
        if results_csv.exists():
            return results_csv
        
        return backend_results  # Return default location
    
    def clean(self):
        """Clean build artifacts."""
        print("Cleaning build artifacts...")
        subprocess.run(
            ["make", "clean"],
            cwd=self.backend_dir,
            capture_output=True
        )
        print("✓ Clean complete")
