// Taken from
// https://medium.com/@kimlindholm/adding-webpack-3-to-phoenix-e6633dbc2bc4
// TODO: Find out if we need to add popper.js for bootstrap, a la: https://blog.danivovich.com/2017/08/30/webpack-phoenix/
/*
 * Modules
 **/
const path = require("path")
const webpack = require("webpack")
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const CopyWebpackPlugin = require("copy-webpack-plugin")
const autoprefixer = require("autoprefixer")

/*
 * Configuration
 **/
module.exports = (env, argv) => {
  const isDev = !(env && env.prod) && argv.mode !== "production"

  // Turned off uglifying due to minimization of webworker inlining not having 'window' defined
  // See:
  // https://github.com/videojs/videojs-contrib-hls/issues/600
  // https://github.com/video-dev/hls.js/issues/187
  const devtool = isDev ? "eval-source-map" : false

  return {
    devtool: devtool,

    context: __dirname,

    entry: {
      app: ["src/app.js", "src/styles/app.scss"]
    },

    output: {
      path: path.resolve(__dirname, "../priv/static"),
      filename: "js/[name].js",
      publicPath: isDev ? "http://localhost:4000/" : "/"
    },

    devServer: {
      disableHostCheck: true,
      host: "localhost",
      headers: {
        "Access-Control-Allow-Origin": "*"
      }
    },

    module: {
      rules: [
        {
          test: /\.(jsx?)$/,
          exclude: /node_modules/,
          use: {
            loader: "babel-loader"
          }
        },

        {
          test: /\.(gif|png|jpe?g|svg)$/i,
          exclude: /node_modules/,
          use: [
            "file-loader?name=images/[name].[ext]",
            {
              loader: "image-webpack-loader",
              options: {
                query: {
                  mozjpeg: {
                    progressive: true
                  },
                  gifsicle: {
                    interlaced: true
                  },
                  optipng: {
                    optimizationLevel: 7
                  },
                  pngquant: {
                    quality: "65-90",
                    speed: 4
                  }
                }
              }
            }
          ]
        },

        {
          test: /\.(ttf|woff2?|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          exclude: /node_modules/,
          query: { name: "fonts/[hash].[ext]" },
          loader: "file-loader"
        },
        {
          test: /\.(c|sc)ss$/,
          exclude: /node_modules\/(?!(video.js)\/).*/,
          use: [
            {
              loader: MiniCssExtractPlugin.loader,
              options: {
                hmr: process.env.NODE_ENV === "development",
                reloadAll: true
              }
            },
            "css-loader",
            "postcss-loader",
            {
              loader: "sass-loader",
              options: {
                sassOptions: {
                  includePaths: [path.resolve("node_modules/bootstrap/scss")]
                },
                sourceMap: isDev
              }
            }
          ]
        },

        {
          test: /\.swf$/,
          loader: "file-loader",
          query: {
            name: "static/media/[name].[ext]"
          }
        },
        {
          test: /\.css$/i,
          use: ["style-loader", "css-loader"]
        }
      ]
    },

    resolve: {
      modules: ["node_modules", __dirname],
      extensions: [".mjs", ".js", ".json", ".jsx", ".css", ".scss"],
      alias: {
        components: path.resolve(__dirname, "src/components/"),
        contexts: path.resolve(__dirname, "src/contexts/"),
        images: path.resolve(__dirname, "src/images/"),
        mutations: path.resolve(__dirname, "src/mutations/"),
        queries: path.resolve(__dirname, "src/queries/"),
        styles: path.resolve(__dirname, "src/styles/"),
        types: path.resolve(__dirname, "src/types/"),
        utils: path.resolve(__dirname, "src/utils/")
      }
    },

    plugins: [
      new MiniCssExtractPlugin({
        filename: "css/[name].css",
        allChunks: true
      }),
      new webpack.DefinePlugin({
        ENV: {
          DEVELOPMENT: isDev,
          S3_BUCKET: JSON.stringify(
            process.env.S3_BUCKET || "streaming-orcasound-net"
          ),
          SHOW_PLAYER_DEBUG_INFO: process.env.SHOW_PLAYER_DEBUG_INFO || isDev,
          ENV_NAME: JSON.stringify(process.env.ENV_NAME || ""),
          GOOGLE_ANALYTICS_ID: JSON.stringify(
            process.env.GOOGLE_ANALYTICS_ID || ""
          ),
          FEATURE_ACTIVITY_BUTTON: process.env.FEATURE_ACTIVITY_BUTTON || isDev
        },
        "process.env": {
          // defaults the environment to development if not specified
          NODE_ENV: JSON.stringify(process.env.NODE_ENV || "development")
        }
      }),

      new CopyWebpackPlugin([
        {
          from: "./static",
          to: path.resolve(__dirname, "../priv/static")
        }
      ])
    ]
  }
}
