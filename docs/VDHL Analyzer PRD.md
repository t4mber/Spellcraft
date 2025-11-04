# **PRD: VDHL Hardware-Safety Static Analyzer (Prototype)**

Status: Draft  
Owner: Product Team  
Reviewers: Engineering (FPGA), Engineering (Compiler)

## **1\. Introduction & Problem Statement**

### **1.1 The Problem**

Hardware developers writing VDHL often lack tools for *pre-synthesis* safety checking. Critical hardware constraints—such as maximum clock frequencies for specific components, thermal limits, or safe fan-out—are typically only caught during synthesis, in costly simulation, or worse, during physical testing. These late-stage discoveries can lead to board respins and, in the worst case, physically damaged ("burnt out") hardware components.

### **1.2 The Vision**

A static analysis tool that reads VDHL source code and uses a sophisticated type system to model and verify physical hardware constraints *before* synthesis. By identifying constraint violations at design time, we can provide immediate feedback to developers, preventing hardware-damaging code from ever reaching a physical device.

### **1.3 Example**

A developer instantiates a PLL to multiply a 50MHz pixel clock by 4.16, producing a 208MHz clock. They then route this clock directly to an analog YPbPr video encoder that has a specified maximum operating frequency of 165MHz. This "over-clocking" could damage the encoder. Our tool should detect this connection and flag it as a critical error.

## **2\. Objectives (Prototype)**

This document describes a *prototype* intended to prove the viability of this approach.

* **Objective 1:** Demonstrate the ability to parse a subset of VDHL (component instantiations, generic maps, port maps) and build a model of the design's connectivity.  
* **Objective 2:** Prove that Haskell's dependent type features (e.g., DataKinds, GADTs, TypeFamilies) can effectively model physical constraints like clock frequencies as type-level values.  
* **Objective 3:** Implement a "constraint-aware" analysis pass that identifies at least two classes of critical hardware-safety violations.  
* **Objective 4:** Provide clear, actionable error messages to the user, pinpointing the problematic VDHL code.

## **3\. Scope**

### **3.1 In Scope**

* **VDHL Subset:** Parsing of entity, architecture, component instantiation, generic map, and port map.  
* **Constraint Definition:** A simple, internal "component library" (defined in Haskell) where constraints for a few test components (e.g., PLL\_1, YPbPr\_Encoder\_A) can be specified (e.g., max\_input\_freq \= 165).  
* **Analysis 1: Parameter Range Checking:** Identify parameter values (from generic map) that exceed the component's defined safe ranges.  
* **Analysis 2: Clock-Domain Propagation:** Basic tracking of clock frequencies as they are propagated from a source (e.g., a PLL output) to a sink (e.g., an encoder input).  
* **Analysis 3: Combinatorial Path Detection:** A heuristic-based check to identify potential "combinatorial explosions" (e.g., a chain of \> N un-clocked arithmetic operations in a single process).

### **3.2 Out of Scope**

* Full VDHL-2008 syntax compliance.  
* A user-facing language for defining constraints (constraints will be hard-coded in Haskell for the prototype).  
* Integration with any vendor synthesis tool (e.g., Vivado, Quartus).  
* Analysis of post-synthesis netlists or timing reports. This is purely a pre-synthesis, source-code-level tool.  
* Behavioral simulation or formal verification of logical correctness (the focus is on *physical* safety).

## **4\. Key Features & User Stories**

| Feature ID | User Story | Acceptance Criteria (Prototype) |
| :---- | :---- | :---- |
| **F-01** | **Frequency Constraint Violation** | As a developer, I want to be warned if I connect a signal from a PLL's output port to an encoder's input port, where the PLL's generic map causes its output frequency to exceed the encoder's known max\_input\_freq. |
| **F-02** | **Dependent Type Modeling** | As a tool developer, I want to use Haskell's type system to represent a signal as Signal (Clock (Freq 208 "MHz")) and an input port as Input (MaxFreq 165 "MHz"), so that the analyzer's internal type-checker flags their connection as an error. |
| **F-03** | **Combinatorial Explosion** | As a developer, I want to be warned if I write a process that chains 4 or more multipliers and adders together without an intermediate clock edge, as this is likely to cause a timing failure or excessive power draw. |
| **F-04** | **Clear Error Reporting** | As a developer, when a violation is found, I want a command-line message that states the error (e.g., "Frequency Violation: 208MHz \> 165MHz") and points to the VDHL file and line number of the problematic component instantiation. |

## **5\. Technical Requirements**

* **Implementation Language:** Haskell (GHC), heavily utilizing extensions like DataKinds, GADTs, TypeFamilies, and KindSignatures.  
* **Input:** One or more .vhd source files.  
* **Output:** Standard output (stdout/stderr) messages.  
* **Dependencies:** A VDHL parsing library (e.g., language-vdhl or a custom parser built with megaparsec).

## **6\. Success Criteria (Prototype)**

The prototype will be considered a success if:

1. It successfully parses a test project containing the VDHL files for the PLL / Encoder scenario.  
2. It correctly identifies and flags the 208MHz clock-over-driving violation as described in **F-01**.  
3. It correctly identifies and flags a separate test case with a long combinatorial path as described in **F-03**.  
4. The internal Haskell implementation clearly demonstrates the use of type-level values (via DataKinds) to represent and check physical constraints.