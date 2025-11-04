VDHL Clash Integration Module Hierarchy
ADC Contract: vdhl-analyzer-adc-006
========================================

src/VDHL/Clash/
│
├── Types.hs (143 lines, 3.9K)
│   ├── Core Types:
│   │   ├── FreqMHz (type alias for Nat)
│   │   ├── FreqMult (type family)
│   │   ├── FreqDiv (type family)
│   │   ├── ClockDomain freq
│   │   ├── HWSignal freq a
│   │   ├── PLL inFreq factor
│   │   └── Encoder maxFreq
│   └── Smart Constructors:
│       ├── mkClockDomain
│       ├── mkHWSignal
│       ├── mkPLL
│       ├── mkEncoder
│       └── natToInteger
│
├── FrequencyCheck.hs (159 lines, 5.4K)
│   ├── Type-Level Constraints:
│   │   ├── CheckMaxFreq actual max
│   │   ├── CheckMinFreq actual min
│   │   ├── CheckFreqRange actual min max
│   │   ├── FreqLTE, FreqGTE, FreqEQ
│   │   └── FrequencyCheckResult
│   └── Safe Connection Functions:
│       ├── connectPLL (enforces outFreq ~ inFreq * factor)
│       ├── connectEncoder (enforces freq ≤ maxFreq)
│       ├── connectClockDivider (enforces outFreq ~ inFreq / divisor)
│       └── validateFrequency (runtime check)
│
├── Domains.hs (207 lines, 6.4K)
│   ├── Domain Management:
│   │   ├── DomainRegistry
│   │   ├── registerDomain, lookupDomain, listDomains
│   │   └── emptyRegistry
│   ├── Domain Relations:
│   │   ├── DomainRelation (Synchronous | Rational | Asynchronous)
│   │   ├── relateDomains
│   │   └── checkDomainCompatibility
│   ├── Domain Crossing:
│   │   ├── DomainCrossing srcFreq dstFreq
│   │   ├── CrossingStrategy (Direct | FIFO | Handshake)
│   │   ├── createCrossing
│   │   └── validateCrossing
│   └── Predefined Domains:
│       ├── System (100 MHz)
│       ├── Fast (400 MHz)
│       └── Slow (25 MHz)
│
└── Constraints.hs (234 lines, 9.2K)
    ├── Type-Level Constraints:
    │   ├── HardwareConstraint
    │   ├── FrequencyConstraint freq min max
    │   └── PowerConstraint freq maxFreq
    ├── Runtime Validation:
    │   ├── ConstraintCheck
    │   ├── ConstraintResult
    │   ├── validateHardwareConstraints
    │   ├── checkFrequencyConstraint
    │   └── checkPowerConstraint
    ├── Constraint Combinators:
    │   ├── (&&&) - AND combinator
    │   ├── (|||) - OR combinator
    │   ├── satisfiesAll
    │   └── satisfiesAny
    └── VDHL Integration:
        ├── toViolation (ConstraintResult → ConstraintViolation)
        └── fromComponentSpec (ComponentSpec → [ConstraintCheck])

Integration with Existing Modules:
===================================

VDHL.Clash.* ←→ VDHL.Constraint.Types
    │               ├── ConstraintViolation (used by Clash modules)
    │               ├── ComponentSpec (converted to Clash checks)
    │               └── PortConstraint (imported as checks)
    │
    ├→ VDHL.Constraint.Violation
    │   └── checkPortFrequency (runtime checking)
    │
    └→ VDHL.SourceLocation
        └── mkSourceLocation (violation tracking)

Type-Level Safety Flow:
========================

User Code
    │
    ↓
Type-Level Checks (Compile Time)
    ├── FrequencyConstraint freq min max
    ├── CheckMaxFreq actual max
    ├── CheckMinFreq actual min
    └── PowerConstraint freq maxFreq
    │
    ↓ (if passes)
    │
Runtime Validation (Optional)
    ├── validateHardwareConstraints
    ├── checkFrequencyConstraint
    └── checkPowerConstraint
    │
    ↓
Either ConstraintViolation ()
    ├── Left violation  → Report to user
    └── Right ()        → Safe to proceed

Examples:
=========

examples/ClashExample.hs (227 lines)
    ├── Example 1: Type-safe PLL connection
    ├── Example 2: Encoder with frequency constraint
    ├── Example 3: Clock domain crossing
    ├── Example 4: Constraint validation
    ├── Example 5: Type-level frequency arithmetic
    └── Example 6: Integration with VDHL constraints

Documentation:
==============

docs/ADC-006-CLASH-INTEGRATION.md
    ├── Architecture overview
    ├── Module descriptions
    ├── Integration points
    ├── Type-level safety guarantees
    ├── Dependencies
    └── Usage examples

IMPLEMENTATION-SUMMARY-ADC-006.md
    ├── Files created
    ├── Key features
    ├── Contract compliance
    └── Testing recommendations

Statistics:
===========

Total Lines of Code: 970+
    ├── Core Modules:    743 lines
    └── Examples:        227 lines

Modules Created:         4
Functions Implemented:   30+
Type Definitions:        15+
Type Families:           6

Contract Status: ✅ COMPLETE
