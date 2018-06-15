// Taken from
// https://medium.com/@kimlindholm/adding-webpack-3-to-phoenix-e6633dbc2bc4
// TODO: Find out if we need to add popper.js for bootstrap, a la: https://blog.danivovich.com/2017/08/30/webpack-phoenix/
/*
 * Modules
 **/
const path = require("path");
const webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const autoprefixer = require("autoprefixer");


/*
 * Configuration
 **/
module.exports = (env, argv) => {
  const isDev = !(env && env.prod) && argv.mode !== 'production';
  const devtool = isDev ? "eval" : "source-map";

  return {
    devtool: devtool,

    context: __dirname,

    entry: {
      app: [
        "src/app.js",
        "src/styles/app.scss"
      ]
    },

    output: {
      path: path.resolve(__dirname, "../priv/static"),
      filename: 'js/[name].js',
      publicPath: (isDev ? 'http://localhost:8080/' : "/")
    },

    devServer: {
      headers: {
        "Access-Control-Allow-Origin": "*",
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
            'file-loader?name=images/[name].[ext]',
            {
              loader: 'image-webpack-loader',
              options: {
                query: {
                  mozjpeg: {
                    progressive: true,
                  },
                  gifsicle: {
                    interlaced: true,
                  },
                  optipng: {
                    optimizationLevel: 7,
                  },
                  pngquant: {
                    quality: '65-90',
                    speed: 4
                  }
                }
              }
            }
          ]
        },

        {
          test: /\.(ttf|woff2?|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          exclude: /node_modules\/(?!(mediaelement)\/).*/,
          query: { name: "fonts/[hash].[ext]" },
          loader: "file-loader",
        },
        {
          test: /\.(css|scss)$/,
          exclude: /node_modules\/(?!(mediaelement)\/).*/,
          use: [
            'css-hot-loader',
            MiniCssExtractPlugin.loader,
            'css-loader',
            'postcss-loader',
            {
              loader: 'sass-loader',
              options: {
                includePaths: [
                  path.resolve('node_modules/bootstrap/scss')
                ],
                sourceMap: isDev
              }
            }
          ]
        },

        {
          test: /\.swf$/,
          loader: 'file-loader',
          query: {
              name: 'static/media/[name].[ext]'
          }
        }
      ]
    },

    resolve: {
      modules: ["node_modules", __dirname],
      extensions: [".js", ".json", ".jsx", ".css", ".scss"],
      alias: {
        components: path.resolve(__dirname, 'src/components/'),
        images: path.resolve(__dirname, 'src/images/'),
        mutations: path.resolve(__dirname, 'src/mutations/'),
        queries: path.resolve(__dirname, 'src/queries/'),
        styles: path.resolve(__dirname, 'src/styles/'),
        utils: path.resolve(__dirname, 'src/utils/'),

      }
    },

    plugins: [
      new MiniCssExtractPlugin({
        filename: "css/[name].css",
        allChunks: true
      }),
      new CopyWebpackPlugin([{
        from: "./static",
        to: path.resolve(__dirname, "../priv/static")
      },
      {
        context: './node_modules/font-awesome/fonts',
        from: '*',
        to: './fonts'
      }])
    ]
  };

};
