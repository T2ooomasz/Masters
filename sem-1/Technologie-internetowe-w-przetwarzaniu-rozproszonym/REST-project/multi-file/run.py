# Import the application factory
from app import create_app
# Import the data initialization function
from app.sample_data import initialize_sample_data
# Import data for the startup message
from app.data import authors, books

# Initialize the sample data
initialize_sample_data()

# Create the Flask app instance using the factory
app = create_app()

# This block runs only when the script is executed directly
if __name__ == '__main__':
    
    print(app.url_map)
    # Run the development server
    app.run(debug=True, host='0.0.0.0', port=5000)
