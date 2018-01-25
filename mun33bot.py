"""Test app for Google OAuth"""
# pylint:disable=no-member
import os
import io
import math
import json
import flask

import google.oauth2.credentials
import google_auth_oauthlib.flow
import googleapiclient.discovery
from googleapiclient.http import MediaIoBaseUpload

APP = flask.Flask(__name__)
APP.secret_key = 'a somewhat secret development key'

CLIENT_SECRETS_FILE = os.path.join(APP.root_path, 'mun33bot_secret.json')

SCOPES = ['https://www.googleapis.com/auth/drive.appdata']
API_SERVICE_NAME = 'drive'
API_VERSION = 'v3'

JSON_FILENAME = 'mun33.json'

@APP.route('/')
def index():
    return flask.send_file('index.html')

@APP.route('/mun33bot.css')
def index_css():
    return flask.send_file('mun33bot.css')

@APP.route('/mun33bot.js')
def index_js():
    return flask.send_file('mun33bot.js')

@APP.route('/getState', methods=['GET', 'POST'])
def get_state():
    if 'credentials' not in flask.session:
        return flask.redirect('authorize')
    credentials = google.oauth2.credentials.Credentials(**flask.session['credentials'])
    flask.session['credentials'] = credentials_to_dict(credentials)
    drive = googleapiclient.discovery.build(API_SERVICE_NAME, API_VERSION, credentials=credentials)
    state = get_state_data(drive)
    if flask.request.method == 'POST':
        incoming = flask.request.get_json()
        print('incoming = {}'.format(incoming))
        state = incoming
        set_state_data(drive, state)
    return flask.jsonify(state)

@APP.route('/authorize')
def authorize():
    """Login route"""
    flow = google_auth_oauthlib.flow.Flow.from_client_secrets_file(
        CLIENT_SECRETS_FILE, scopes=SCOPES)
    flow.redirect_uri = flask.url_for('auth', _external=True)
    authorization_url, state = flow.authorization_url(
        access_type='offline',
        include_granted_scopes='true')
    flask.session['state'] = state
    return flask.redirect(authorization_url)

@APP.route('/auth')
def auth():
    """authorization callback"""
    state = flask.session['state']
    flow = google_auth_oauthlib.flow.Flow.from_client_secrets_file(
        CLIENT_SECRETS_FILE, scopes=SCOPES, state=state)
    flow.redirect_uri = flask.url_for('auth', _external=True)
    authorization_response = flask.request.url
    flow.fetch_token(authorization_response=authorization_response)
    credentials = flow.credentials
    flask.session['credentials'] = credentials_to_dict(credentials)
    return flask.redirect(flask.url_for('/'))

def credentials_to_dict(credentials):
    """turn credentials into dictionary"""
    return {'token': credentials.token,
            'refresh_token': credentials.refresh_token,
            'token_uri': credentials.token_uri,
            'client_id': credentials.client_id,
            'client_secret': credentials.client_secret,
            'scopes': credentials.scopes}

def get_state_data(drive):
    """get state data from Google Drive"""
    response = drive.files().list(
        orderBy='viewedByMeTime desc',
        spaces='appDataFolder',
        q="name = '{}'".format(JSON_FILENAME)).execute()
    try:
        flask.session['fileId'] = response['files'][0]['id']
        for dup in response['files'][1:]:
            drive.files().delete(fileId=dup['id']).execute()
    except IndexError:
        metadata = {'name': JSON_FILENAME, 'parents': ['appDataFolder']}
        state_data = io.BytesIO(bytes(json.dumps(json.loads('{}')), encoding='utf-8'))
        media = MediaIoBaseUpload(state_data, mimetype='application/json', resumable=True)
        flask.session['fileId'] = drive.files().create(body=metadata, media_body=media, fields='id').execute()['id']
    return json.loads(drive.files().get_media(fileId=flask.session['fileId']).execute().decode('utf-8'))

def set_state_data(drive, state):
    """save state to Google Drive"""
    data = io.BytesIO(bytes(json.dumps(state), encoding='utf-8'))
    media = MediaIoBaseUpload(data, mimetype='application/json', resumable=True)
    drive.files().update(fileId=flask.session['fileId'], media_body=media).execute()

if __name__ == '__main__':
    # When running locally, disable OAuthlib's HTTPs verification.
    # ACTION ITEM for developers:
    #     When running in production *do not* leave this option enabled.
    os.environ['OAUTHLIB_INSECURE_TRANSPORT'] = '1'

    APP.run('127.0.0.1', 5000, debug=True)
