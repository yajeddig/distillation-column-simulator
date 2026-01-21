"""
Distillation Column Simulator - Python Wrapper

This package provides a Python interface to the Fortran-based
distillation column simulator, enabling easy integration with
modern Python workflows and web applications.

Author: Younes AJEDDIG
Based on: ENSGTI 2018 project (original Fortran implementation)
"""

__version__ = "2.0.0"
__author__ = "Younes AJEDDIG"

from .fortran_interface import DistillationSimulator
from .config_manager import ConfigManager
from .data_processor import ResultsProcessor

__all__ = [
    'DistillationSimulator',
    'ConfigManager',
    'ResultsProcessor',
]
