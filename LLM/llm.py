import os
import sys
from google import genai
import numpy as np
from dotenv import load_dotenv
import json

# print(f"Python executable: {sys.executable}")
# print(f"Python version: {sys.version}")

load_dotenv()
seam_api_key = os.getenv("SEAM_API_KEY")

client = genai.Client(api_key=seam_api_key)

def llm_output(pitcher, hitter):

    with open("LLM/seam_data.json", "r") as f:
        seam_data = json.load(f)
    
    z_values = [point.get('z', 0) for point in seam_data]
    threshold = np.percentile(z_values, 96)    # top 4% of density.
    
    high_density = [point for point in seam_data if point.get('z', 0) > threshold]

    prompt = f"""
    
    You are a baseball analyst, given SEAM data, a predictive batted-ball distrubution of the matchup with coordinates (x, y) on a field with density (z).
    Data contains the highest density regions, the regions of the field where the ball is most likely to land for the matchup between hitter="{hitter}" and pitcher="{pitcher}".
    Reference https://github.com/ecklab/seam-manuscript/blob/main/seam.pdf

    IMPORTANT: only use data from https://baseballsavant.mlb.com/

    SEAM data (highest density regions): {high_density}

    Requirements:
        - only include 2-3 bullet points, no text above or below it
        - 100-150 characters for each bullet point
        - add a new line after each bullet point
        - include 2-3 player tendencies
        - trendencies provide evidence for SEAM data... example, say "[tendency] aligns with SEAM data..." not "SEAM data aligns with [tendency]"
    
    Bullet point ideas:
        - hitter's statcast tendencies
        - pitcher's statcast tendencies
        - defensive positioning recommendations
    
    IMPORTANT: don't mention stats or percentages, only tendencies (Player A had high pull rate... don't say Player A had 12% pull rate)

    Example, hitter="Javier Baez", pitcher="Matthew Boyd":

        * Baez's high chase % throughout his career, often resulting in weak contact, corresponds with SEAM's highest density region lying in the infield.

        * Baez's low 2025 barrel % correlates to SEAM data showing high densities in the infield and shallow outfield.

        * Consider positioning outfielders in the shallow outfield to account for Baez's tendency for weak contact.
    """

    try:
        response = client.models.generate_content(
            model="gemini-3.1-flash-lite-preview",    # NOTE: probably best option is "gemini-3.1-flash-lite-preview"
            contents=prompt    # FIXME: change during deployment.
        )

        return response.text
    except Exception as e:
        return '**Error:** API is currently experiencing high demand.'

if __name__ == "__main__":
    print(llm_output(sys.argv[1], sys.argv[2]))

# # ---- TESTING ----
# print(llm_output("Shohei Ohtani", "Clayton Kershaw"))


    # Example, hitter="Pete Crow-Armstrong", pitcher="Paul Skenes":    # NOTE: had to take out this example from the LLM prompt due to rate limits.

    #     * Skene's low hard-hit % aligns with SEAM's highest density region lying in the infield.

    #     * Crow Armstrong's high ground ball tendencies aligns with SEAM's data showing it's higest density in the infield.

    #     * Consider defensive shifts to towards the 1B side, as SEAM data indicates a high % of batted balls toward PCA's pull side.