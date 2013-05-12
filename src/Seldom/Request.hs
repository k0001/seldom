{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- Some code in this file was originally from wai-1.3.0.3, made
-- available under the following terms (MIT):
--
-- Copyright (c) 2012 Michael Snoyman, http://www.yesodweb.com/
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------

module Seldom.Request (
    RequestMsg(..)
  , Request(..)
  ) where


import           Data.Typeable                 (Typeable)
import qualified Network.Socket                as NS
import qualified Data.ByteString.Char8         as B
import qualified Network.HTTP.Types            as H

--------------------------------------------------------------------------------

data RequestMsg = RequestMsg
  {  reqmMethod         :: !H.StdMethod
  ,  reqmHttpVersion    :: !H.HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
  -- depending on backend; in a standalone server setting, this is most likely
  -- all information after the domain name. In a CGI application, this would be
  -- the information following the path to the CGI executable itself.
  -- Do not modify this raw value- modify pathInfo instead.
  ,  reqmRawPathInfo    :: !B.ByteString
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value- modify queryString instead.
  ,  reqmRawQueryString :: !B.ByteString
  ,  reqmHeaders        :: !H.RequestHeaders
  } deriving (Eq, Ord, Show, Typeable)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
data Request = Request {
  -- | The request message data.
     reqMsg         :: !RequestMsg
  -- | Generally the host requested by the user via the Host request header.
  -- Backends are free to provide alternative values as necessary. This value
  -- should not be used to construct URLs.
  ,  reqServerName  :: !B.ByteString
  -- | The listening port that the server received this request on. It is
  -- possible for a server to listen on a non-numeric port (i.e., Unix named
  -- socket), in which case this value will be arbitrary. Like 'serverName',
  -- this value should not be used in URL construction.
  ,  reqServerPort  :: !B.ByteString
  -- | Was this request made over an SSL/TLS connection?
  ,  reqIsSecure    :: !Bool
  -- | The client\'s host information.
  ,  reqRemoteHost  :: !NS.SockAddr
  } deriving (Eq, Show, Typeable)