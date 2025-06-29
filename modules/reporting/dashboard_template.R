# Dashboard HTML Template
# Author: Konrad Gorzelańczyk

get_dashboard_html_template <- function() {
  return('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Statistical Analysis Dashboard</title>
    <link href="https://fonts.googleapis.com/css2?family=SF+Pro+Display:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css" rel="stylesheet">
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, "SF Pro Display", "Segoe UI", Roboto, sans-serif;
            background: #f8f9fa;
            min-height: 100vh;
            color: #1d1d1f;
            line-height: 1.6;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            padding: 60px 20px;
        }
        
        .header {
            text-align: center;
            margin-bottom: 80px;
        }
        
        .header h1 {
            font-size: 3.5rem;
            font-weight: 700;
            color: #1d1d1f;
            margin-bottom: 16px;
            letter-spacing: -0.05em;
        }
        
        .header .subtitle {
            font-size: 1.5rem;
            font-weight: 400;
            color: #6e6e73;
            margin-bottom: 12px;
        }
        
        .header .meta {
            font-size: 1rem;
            color: #86868b;
            font-weight: 400;
        }
        
        .analysis-grid {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 32px;
            margin-bottom: 60px;
        }
        
        .analysis-card {
            background: white;
            border-radius: 18px;
            padding: 0;
            border: 1px solid #e5e5e7;
            transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
            overflow: hidden;
            position: relative;
        }
        
        .analysis-card:hover {
            transform: translateY(-8px);
            box-shadow: 0 25px 50px rgba(0, 0, 0, 0.1);
            border-color: #d2d2d7;
        }
        
        .card-icon {
            padding: 40px 32px 24px;
            text-align: center;
            border-bottom: 1px solid #f5f5f7;
        }
        
        .card-icon i {
            font-size: 2.5rem;
            margin-bottom: 16px;
            display: block;
        }
        
        .card-icon.primary i { color: #007aff; }
        .card-icon.success i { color: #34c759; }
        .card-icon.warning i { color: #ff9500; }
        .card-icon.info i { color: #5856d6; }
        
        .card-icon h3 {
            font-size: 1.375rem;
            font-weight: 600;
            color: #1d1d1f;
            margin-bottom: 8px;
        }
        
        .card-content {
            padding: 24px 32px 32px;
            text-align: center;
        }
        
        .card-content p {
            font-size: 1rem;
            color: #6e6e73;
            margin-bottom: 24px;
            line-height: 1.5;
        }
        
        .btn-analysis {
            display: inline-block;
            background: #1d1d1f;
            color: white;
            text-decoration: none;
            padding: 12px 24px;
            border-radius: 50px;
            font-size: 0.95rem;
            font-weight: 500;
            transition: all 0.2s ease;
            border: none;
            cursor: pointer;
            min-width: 140px;
        }
        
        .btn-analysis:hover {
            background: #424245;
            color: white;
            text-decoration: none;
            transform: scale(1.02);
        }
        
        .btn-analysis:disabled {
            background: #d2d2d7;
            color: #86868b;
            cursor: not-allowed;
            transform: none;
        }
        
        .btn-analysis i {
            margin-right: 8px;
        }
        
        .footer-note {
            text-align: center;
            margin-top: 40px;
        }
        
        .footer-note p {
            font-size: 1rem;
            color: #86868b;
            font-weight: 400;
        }
        
        .footer-note i {
            margin-right: 8px;
            color: #007aff;
        }
        
        .creator-credit {
            position: fixed;
            bottom: 20px;
            right: 20px;
            background: rgba(255, 255, 255, 0.9);
            padding: 8px 16px;
            border-radius: 20px;
            border: 1px solid #e5e5e7;
            backdrop-filter: blur(10px);
        }
        
        .creator-credit p {
            font-size: 0.85rem;
            color: #6e6e73;
            margin: 0;
            font-weight: 400;
        }
        
        @media (max-width: 1200px) {
            .analysis-grid {
                grid-template-columns: repeat(2, 1fr);
                gap: 24px;
            }
        }
        
        @media (max-width: 768px) {
            .container {
                padding: 40px 16px;
            }
            
            .header h1 {
                font-size: 2.5rem;
            }
            
            .header .subtitle {
                font-size: 1.25rem;
            }
            
            .analysis-grid {
                grid-template-columns: 1fr;
                gap: 24px;
            }
            
            .creator-credit {
                position: relative;
                bottom: auto;
                right: auto;
                margin-top: 40px;
                text-align: center;
                background: transparent;
                border: none;
                padding: 0;
            }
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Statistical Analysis Dashboard</h1>
            <p class="subtitle">Complete Statistical Data Analysis Suite</p>
            <p class="meta">Generated: %s | Dataset: %d observations, %d variables</p>
        </div>
        
        <div class="analysis-grid">
            <div class="analysis-card">
                <div class="card-icon primary">
                    <h3>Descriptive Statistics</h3>
                </div>
                <div class="card-content">
                    <p>Summary statistics, distributions, and data quality assessment</p>
                    %s
                </div>
            </div>
            
            <div class="analysis-card">
                <div class="card-icon success">
                    <h3>Correlation Analysis</h3>
                </div>
                <div class="card-content">
                    <p>Variable relationships and correlation matrices</p>
                    %s
                </div>
            </div>
            
            <div class="analysis-card">
                <div class="card-icon warning">
                    <h3>Comparative Analysis</h3>
                </div>
                <div class="card-content">
                    <p>Group comparisons and statistical tests</p>
                    %s
                </div>
            </div>
            
            <div class="analysis-card">
                <div class="card-icon info">
                    <h3>Enhanced Inferential</h3>
                </div>
                <div class="card-content">
                    <p>Advanced modeling with covariates and interactions</p>
                    %s
                </div>
            </div>
        </div>
        
        <div class="creator-credit">
            <p>Created by Konrad Gorzelańczyk</p>
        </div>
    </div>
</body>
</html>')
}

# Helper function to generate the complete dashboard HTML
generate_dashboard_html <- function(timestamp, n_rows, n_cols, desc_button, corr_button, comp_button, infer_button) {
  template <- get_dashboard_html_template()
  return(sprintf(template, timestamp, n_rows, n_cols, desc_button, corr_button, comp_button, infer_button))
} 