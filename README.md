# Biodiversa+ Habitat Pilot: Technical Supplement to Czech Report

This repository contains scripts and workflows developed for the **Biodiversa+ Habitat pilot**. It serves as a **technical supplement** to the *Biodiversa+ Habitat Pilot Czech Report (January 2026)*, documenting Remote Sensing methods for monitoring wetland and grassland ecosystems in the Czechia.

---

## Ownership & Attribution

**Ownership:** All scripts and methodologies are the property of the **Habitat pilot project team**.

* **Algorithms:** Inundation mapping based on Jussila et al. (2024) and Lefebvre et al. (2019).
* **Data:** Sentinel-2 imagery and EU Grassland Watch (EUGW) products.
* **Project Role:** Technical implementation, local optimization, and validation (Třeboňsko PLA and Milovice-Mladá).

---

## Subtasks Overview

### 1. Inundation mapping
Implementation and testing of binary decision trees for flooded area detection using Sentinel-2 data.
* **Platform:** Google Earth Engine (JavaScript API).

### 2. Indicators for Habitat condition monitoring
Extension of static inundation mapping into a dynamic monitoring pipeline for long-term trends.
* **Platform:** R (utilizing **openEO**).

### 3. EU Grassland Watch
Validation of Land Cover (LC) and GrasslandType (GT) components (EUNIS 2021 Level 2).
* **Platform:** R and Jupyter Notebooks.

 ---

## Contacts

For technical questions regarding the implementation and workflows, please contact:

* **Vít Ježek** – [@jezekvi-github-link](https://github.com/jezekvi)
* **Jakub Rataj** – [@zbubster-github-link](https://github.com/zbubster)
 
