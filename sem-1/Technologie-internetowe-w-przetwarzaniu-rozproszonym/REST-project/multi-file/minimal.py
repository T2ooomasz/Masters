from flask import Flask
app = Flask(__name__)

@app.route('/test')
def test():
    return "Minimal test OK", 200

@app.route('/api/v1/authors')
def authors():
    return {"data": []}, 200

if __name__ == '__main__':
    print(app.url_map)
    app.run(debug=True, port=5001)