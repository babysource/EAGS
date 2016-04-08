using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace ERLDaemon
{
    public partial class Daemon : Form
    {
        private const string EAGS_FILE = "EAGS.bat";

        private const string EOUT_FILE = "EOUT.log";

        // 当前进程
        private Process process = null;

        public Daemon()
        {
            InitializeComponent();
            {
                if (!File.Exists(EOUT_FILE))
                {
                    File.CreateText(EOUT_FILE).Close();
                }
            }
            this.Text += " - " + (
                this.notifyIcon.Text = (new DirectoryInfo(Directory.GetCurrentDirectory())).Name
            );
        }

        private void Launch()
        {
            this.process = new Process();
            {
                this.process.StartInfo.FileName = EAGS_FILE;
                this.process.StartInfo.UseShellExecute = false;
                this.process.StartInfo.RedirectStandardError = true;
                this.process.StartInfo.RedirectStandardInput = false;
                this.process.StartInfo.RedirectStandardOutput = true;
            }
            // 启动命令
            try
            {
                this.process.Start();
            }
            catch
            {
                this.process = null;
            }
            finally
            {
                if (this.process != null && !this.process.HasExited)
                {
                    {
                        this.process.ErrorDataReceived += new DataReceivedEventHandler((sender, e) =>
                        {
                            if (!string.IsNullOrWhiteSpace(e.Data))
                            {
                                this.logBox.BeginInvoke(new Action<string>((info) =>
                                {
                                    this.Logger(info);
                                }), new object[] { e.Data });
                            }
                        });
                        this.process.OutputDataReceived += new DataReceivedEventHandler((sender, e) =>
                        {
                            if (!string.IsNullOrWhiteSpace(e.Data))
                            {
                                this.logBox.BeginInvoke(new Action<string>((info) =>
                                {
                                    this.Logger(info);
                                }), new object[] { e.Data });
                            }
                        });
                    }
                    // 监听输出
                    this.process.BeginErrorReadLine();
                    this.process.BeginOutputReadLine();
                }
            }
        }

        private void Destroy()
        {
            if (this.process != null)
            {
                try
                {
                    this.process.CloseMainWindow();
                    {
                        this.process.Kill();
                    }
                }
                catch { }
                finally
                {
                    this.process = null;
                }
            }
        }

        private void DaemonTimer_Tick(object sender, System.EventArgs e)
        {
            if (this.process == null)
            {
                {
                    this.Logger("开始启动前置机！");
                }
                this.Launch();
            }
            else
            {
                if (this.process.HasExited)
                {
                    this.Logger("尝试重启前置机！");
                    {
                        this.Destroy();
                    }
                    this.Launch();
                }
            }
        }

        private void Daemon_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason == CloseReason.UserClosing && (e.Cancel = true))
            {
                this.WindowState = FormWindowState.Minimized;
                {
                    this.ShowInTaskbar = false;
                }
                this.Hide();
            }
        }

        private void Info_ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.WindowState = FormWindowState.Normal;
            {
                this.ShowInTaskbar = true;
            }
            this.Show();
        }

        private void Exit_ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.Logger("正在退出前置机！");
            {
                this.Destroy();
            }
            // 销毁窗口
            {
                this.notifyIcon.Dispose();
                {
                    Application.Exit();
                }
            }
        }

        private void Logger(string info)
        {
            StringBuilder eout = new StringBuilder(1024);
            {
                eout.Append(DateTime.Now.ToString("[ yyyy/MM/dd HH:mm:ss ]", DateTimeFormatInfo.InvariantInfo));
                eout.Append(" - ");
                eout.Append(info);
                eout.Append(Environment.NewLine);
            }
            this.Logger(eout);
        }

        private void Logger(StringBuilder info)
        {
            string eout = info.ToString();
            {
                if (!string.IsNullOrWhiteSpace(eout))
                {
                    StreamWriter write = null;
                    try
                    {
                        if (File.Exists(EOUT_FILE))
                        {
                            write = File.AppendText(EOUT_FILE);
                        }
                    }
                    catch
                    {
                        write = null;
                    }
                    finally
                    {
                        if (write != null)
                        {
                            try
                            {
                                write.Write(eout);
                            }
                            catch { }
                            finally
                            {
                                write.Close();
                            }
                        }
                    }
                    // 界面输出
                    this.logBox.Text += eout;
                }
            }
        }
    }
}